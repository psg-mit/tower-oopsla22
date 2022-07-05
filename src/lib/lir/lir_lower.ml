open Core
module IdMap = Symbol.Map
module IdSet = Symbol.Set
module Context = Ir.Check.Context
module Lir = Lir_

let rec type_size = function
  | Ast.Tvar _ -> assert false
  | Tunit -> 0
  | Tbool -> 1
  | Tuint | Tptr _ -> !Args.word_size
  | Tprod ts -> List.fold ~init:0 ~f:( + ) (List.map ~f:type_size ts)
;;

let rec lower_typ = function
  | Ast.Tvar _ -> assert false
  | Tunit -> Lir.Tunit
  | Tbool -> Tbool
  | Tuint | Tptr _ -> Tuint
  | Tprod ts -> Tprod (List.map ~f:lower_typ ts)
;;

let rec default = function
  | Lir.Tunit -> Lir.Vunit
  | Tuint -> Vnum 0
  | Tbool -> Vbool false
  | Tprod ts -> Vprod (List.map ~f:default ts)
;;

let rec lower_value = function
  | Ir.Vvar v -> Lir.Vvar v
  | Vunit -> Vunit
  | Vnum i -> Vnum i
  | Vbool b -> Vbool b
  | Vnull -> Vnum 0
  | Vptr _ -> assert false
  | Vprod vs -> Vprod (List.map ~f:lower_value vs)
;;

let lower_exp ctx x = function
  | Ir.Eval v ->
    let t = Ir.Check.synth_value ctx v |> lower_typ in
    [ Lir.Sassign (t, x, Eval (lower_value v)) ]
  | Eproj (y, i) ->
    let p, t =
      match Context.get_local ctx y with
      | Ast.Tprod ts -> List.map ~f:lower_typ ts, lower_typ @@ List.nth_exn ts i
      | _ -> assert false
    in
    [ Sassign (t, x, Eproj (y, p, i)) ]
  | Euop (uop, y) ->
    (match uop with
    | Ast.Unot -> [ Sassign (Tbool, x, Euop (Ulnot, y)) ]
    | Ast.Utest ->
      let t = Context.get_local ctx y |> lower_typ in
      let v = default t in
      let z = Symbol.nonce () in
      [ Sassign (t, z, Eval v)
      ; Sassign (Tbool, x, Ebop (Beq, y, z))
      ; Sunassign (t, z, Eval v)
      ]
    | Ast.Ulnot ->
      let t = Context.get_local ctx y |> lower_typ in
      [ Sassign (t, x, Euop (Ulnot, y)) ])
  | Ebop (bop, y1, y2) ->
    (match bop with
    | Ast.Band -> [ Sassign (Tbool, x, Ebop (Bland, y1, y2)) ]
    | Bor -> [ Sassign (Tbool, x, Ebop (Blor, y1, y2)) ]
    | Bplus -> [ Sassign (Tuint, x, Ebop (Bplus, y1, y2)) ]
    | Bminus -> [ Sassign (Tuint, x, Ebop (Bminus, y1, y2)) ]
    | Btimes -> [ Sassign (Tuint, x, Ebop (Btimes, y1, y2)) ]
    | Beq -> [ Sassign (Tbool, x, Ebop (Beq, y1, y2)) ]
    | Bneq ->
      let z = Symbol.nonce () in
      [ Sassign (Tbool, z, Ebop (Beq, y1, y2))
      ; Sassign (Tbool, x, Euop (Ulnot, z))
      ; Sunassign (Tbool, z, Euop (Ulnot, x))
      ]
    | Bless -> [ Sassign (Tbool, x, Ebop (Bless, y1, y2)) ]
    | Bgreater -> [ Sassign (Tbool, x, Ebop (Bless, y2, y1)) ]
    | Ble ->
      let z = Symbol.nonce () in
      [ Sassign (Tbool, z, Ebop (Bless, y2, y1))
      ; Sassign (Tbool, x, Euop (Ulnot, z))
      ; Sunassign (Tbool, z, Euop (Ulnot, x))
      ]
    | Bge ->
      let z = Symbol.nonce () in
      [ Sassign (Tbool, z, Ebop (Bless, y1, y2))
      ; Sassign (Tbool, x, Euop (Ulnot, z))
      ; Sunassign (Tbool, z, Euop (Ulnot, x))
      ]
    | Bland -> [ Sassign (Tuint, x, Ebop (Bland, y1, y2)) ]
    | Blor -> [ Sassign (Tuint, x, Ebop (Blor, y1, y2)) ]
    | Blsl -> [ Sassign (Tuint, x, Ebop (Blsl, y1, y2)) ]
    | Blsr -> [ Sassign (Tuint, x, Ebop (Blsr, y1, y2)) ])
  | Ealloc t -> [ Sassign (Tuint, x, Ealloc (lower_typ t)) ]
  | Efun _ -> assert false
;;

let rec reverse = function
  | Lir.Sassign (i, x, e) -> Lir.Sunassign (i, x, e)
  | Sunassign (i, x, e) -> Sassign (i, x, e)
  | Sswap (x1, x2) -> Sswap (x1, x2)
  | Smem_swap (x1, x2) -> Smem_swap (x1, x2)
  | Sif (x, s) -> Sif (x, List.rev_map ~f:reverse s)
;;

let rec assigned (add, rem) = function
  | Lir.Sassign (data, key, _) ->
    if IdMap.mem rem key
    then add, IdMap.remove rem key
    else IdMap.add_exn add ~key ~data, rem
  | Sunassign (data, key, _) ->
    if IdMap.mem add key
    then IdMap.remove add key, rem
    else add, IdMap.add_exn rem ~key ~data
  | Sswap _ | Smem_swap _ -> add, rem
  | Sif (_, s) -> List.fold ~init:(add, rem) ~f:assigned s
;;

let rec replace_assign add rem = function
  | Lir.Sassign (t, x, e) when IdSet.mem add x || IdSet.mem rem x ->
    let y = Symbol.nonce () in
    [ Lir.Sassign (t, y, e); Sswap (x, y); Sunassign (t, y, Eval (default t)) ]
  | Sassign (t, x, e) -> [ Sassign (t, x, e) ]
  | Sunassign (t, x, e) when IdSet.mem rem x || IdSet.mem add x ->
    let y = Symbol.nonce () in
    [ Sassign (t, y, Eval (default t)); Sswap (x, y); Sunassign (t, y, e) ]
  | Sunassign (t, x, e) -> [ Sunassign (t, x, e) ]
  | Sswap (x, y) -> [ Sswap (x, y) ]
  | Smem_swap (x, y) -> [ Smem_swap (x, y) ]
  | Sif (x, s) -> [ Sif (x, List.concat_map ~f:(replace_assign add rem) s) ]
;;

let rec lower_stmt ctx = function
  | Ir.Sseq ss ->
    let ctx, ss = List.fold_map ~init:ctx ~f:lower_stmt ss in
    ctx, List.concat ss
  | Sassign (x, e) ->
    Context.add_local ctx x (Ir.Check.synth_exp ctx e), lower_exp ctx x e
  | Sunassign (x, e) ->
    Context.remove_local ctx x, List.rev_map ~f:reverse (lower_exp ctx x e)
  | Sswap (x, y) -> ctx, [ Sswap (x, y) ]
  | Smem_swap (x, y) -> ctx, [ Smem_swap (x, y) ]
  | Sif (x, s1, s2) ->
    let _, s1 = lower_stmt ctx s1 in
    let ctx', s2 = lower_stmt ctx s2 in
    let add, rem = List.fold ~init:(IdMap.empty, IdMap.empty) ~f:assigned s1 in
    let s1 = List.concat_map ~f:(replace_assign (Map.key_set add) (Map.key_set rem)) s1 in
    let s2 = List.concat_map ~f:(replace_assign (Map.key_set add) (Map.key_set rem)) s2 in
    let assign_added =
      Map.to_alist add |> List.map ~f:(fun (v, t) -> Lir.Sassign (t, v, Eval (default t)))
    in
    let unassign_removed =
      Map.to_alist rem
      |> List.map ~f:(fun (v, t) -> Lir.Sunassign (t, v, Eval (default t)))
    in
    let y = Symbol.nonce () in
    let s =
      [ Lir.Sif (x, s1)
      ; Sassign (Tbool, y, Euop (Ulnot, x))
      ; Sif (y, s2)
      ; Sunassign (Tbool, y, Euop (Ulnot, x))
      ]
    in
    ctx', assign_added @ s @ unassign_removed
  | Ir.Sdebug -> ctx, []
;;

let lower_func funcs (f : Ir.func) : Lir.modul =
  let ctx =
    List.fold f.args ~init:(Context.empty funcs) ~f:(fun acc { name; typ } ->
        Context.add_local acc name typ)
  in
  let _, body = lower_stmt ctx f.body in
  let out_arg = lower_typ f.result.typ, f.result.name in
  let args = List.map f.args ~f:(fun a -> lower_typ a.typ, a.name) in
  { name = f.name; out_arg; args; body }
;;
