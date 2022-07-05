open Core
module Ir = Ir_

let rec default = function
  | Ast.Tvar _ -> assert false
  | Tunit -> Ir.Vunit
  | Tbool -> Vbool false
  | Tuint -> Vnum 0
  | Tprod ts -> Vprod (List.map ~f:default ts)
  | Tptr _ -> Vnull
;;

let rec lower_value ctx = function
  | Ast.Vvar i -> Ir.Vvar i
  | Vunit -> Vunit
  | Vnum i -> Vnum i
  | Vbool b -> Vbool b
  | Vnull -> Vnull
  | Vptr (t, p) -> Vptr (Ast.Check.normalize_type ctx t, p)
  | Vprod vs -> Vprod (List.map ~f:(lower_value ctx) vs)
  | Vdefault t -> default (Ast.Check.normalize_type ctx t)
;;

let rec reverse (stmt : Ir.stmt) : Ir.stmt =
  match stmt with
  | Sseq ss -> Sseq (List.rev (List.map ~f:reverse ss))
  | Sassign (x, e) -> Sunassign (x, e)
  | Sunassign (x, e) -> Sassign (x, e)
  | Sswap (x1, x2) -> Sswap (x1, x2)
  | Smem_swap (x1, x2) -> Smem_swap (x1, x2)
  | Sif (x, s1, s2) -> Sif (x, reverse s1, reverse s2)
  | Sdebug -> Sdebug
;;

let flatten (stmt : Ir.stmt) : Ir.stmt =
  let rec flatten' stmt =
    match stmt with
    | Ir.Sseq ss -> List.concat_map ~f:flatten' ss
    | Sif (x, s1, s2) -> [ Ir.Sif (x, Sseq (flatten' s1), Sseq (flatten' s2)) ]
    | s -> [ s ]
  in
  match flatten' stmt with
  | [ s ] -> s
  | ss -> Sseq ss
;;

let rec lower_exp ctx (x : Ir.id) (e : Ast.exp) : Ir.stmt =
  match e with
  | Ast.Eval v -> Sassign (x, Eval (lower_value ctx v))
  | Eprod es ->
    let ys, ss =
      List.map es ~f:(fun e ->
          let y = Symbol.nonce () in
          Ir.Vvar y, lower_exp ctx y e)
      |> List.unzip
    in
    Sseq [ Sseq ss; Sassign (x, Eval (Vprod ys)); reverse (Sseq ss) ]
  | Eproj (v, i) ->
    let y = Symbol.nonce () in
    let s = lower_exp ctx y v in
    Sseq [ s; Sassign (x, Eproj (y, i)); reverse s ]
  | Euop (uop, v) ->
    let y = Symbol.nonce () in
    let s = lower_exp ctx y v in
    Sseq [ s; Sassign (x, Euop (uop, y)); reverse s ]
  | Ebop (bop, v1, v2) ->
    let y1, y2 = Symbol.nonce (), Symbol.nonce () in
    let s1, s2 = lower_exp ctx y1 v1, lower_exp ctx y2 v2 in
    Sseq [ s1; s2; Sassign (x, Ebop (bop, y1, y2)); reverse s2; reverse s1 ]
  | Ealloc t -> Sassign (x, Ealloc (Ast.Check.normalize_type ctx t))
  | Efun { name; bound; args } ->
    let args = List.map args ~f:(lower_value ctx) in
    Sassign (x, Efun { name; bound; args })
;;

let rec desugar_pat x = function
  | Ast.Pid i -> [ Ir.Sassign (i, Eval (Vvar x)); Sunassign (x, Eval (Vvar i)) ]
  | Punit -> [ Sunassign (x, Eval Vunit) ]
  | Pbool b -> [ Sunassign (x, Eval (Vbool b)) ]
  | Puint n -> [ Sunassign (x, Eval (Vnum n)) ]
  | Pnull -> [ Sunassign (x, Eval Vnull) ]
  | Pprod ps ->
    let s, v, rest =
      List.mapi ps ~f:(fun i -> function
        | Pid y -> Ir.Sassign (y, Eproj (x, i)), Ir.Vvar y, []
        | p ->
          let y = Symbol.nonce () in
          Sassign (y, Eproj (x, i)), Vvar y, desugar_pat y p)
      |> List.unzip3
    in
    s @ (Sunassign (x, Eval (Vprod v)) :: List.concat rest)
;;

let rec lower_stmt ctx (stmt : Ast.stmt) : Ir.stmt =
  match stmt with
  | Sseq ss -> Sseq (List.map ss ~f:(lower_stmt ctx))
  | Sassign (Pid i, e) -> lower_exp ctx i e
  | Sassign (pat, e) ->
    let x = Symbol.nonce () in
    let s = lower_exp ctx x e in
    Sseq [ s; Sseq (desugar_pat x pat) ] |> flatten
  | Sunassign (x, e) -> reverse (lower_stmt ctx (Sassign (x, e)))
  | Sswap (x1, x2) -> Sswap (x1, x2)
  | Smem_swap (Eval (Vvar x1), x2) -> Smem_swap (x1, x2)
  | Smem_swap (e1, x2) ->
    let x = Symbol.nonce () in
    let s = lower_exp ctx x e1 in
    Sseq [ s; Smem_swap (x, x2); reverse s ]
  | Sif (x, s1, s2) -> Sif (x, lower_stmt ctx s1, lower_stmt ctx s2)
  | Sdebug -> Sdebug
  | Swith (s1, s2) ->
    let s1 = lower_stmt ctx s1 in
    Sseq [ s1; lower_stmt ctx s2; reverse s1 ]
;;

let lower_func ctx ({ name; bound; args; result; body } : Ast.func) : Ir.func =
  let body = lower_stmt ctx body |> flatten in
  let result, typ = result in
  let typ = Ast.Check.normalize_type ctx typ in
  let args =
    List.map args ~f:(fun i -> { i with typ = Ast.Check.normalize_type ctx i.typ })
  in
  match result with
  | Vvar out -> { name; bound; args; result = { name = out; typ }; body }
  | result ->
    let out = Symbol.nonce () in
    let body = Ir.Sseq [ body; Sassign (out, Eval (lower_value ctx result)) ] in
    { name; bound; args; result = { name = out; typ }; body }
;;

let lower_static ctx ({ name; elt_type; size; values } : Ast.static) : Ir.static =
  let elt_type = Ast.Check.normalize_type ctx elt_type in
  let values =
    match values with
    | Some values -> List.map ~f:(lower_value ctx) values
    | None -> List.init size ~f:(fun _ -> default elt_type)
  in
  { name; elt_type; values }
;;

let lower_decl ctx (decl : Ast.decl) : Ir.decl =
  match decl with
  | Dstatic s -> Dstatic (lower_static ctx s)
  | Dtype t -> Dtype t
  | Dfunc f -> Dfunc (lower_func ctx f)
;;

let lower_decls ctx = List.map ~f:(lower_decl ctx)
