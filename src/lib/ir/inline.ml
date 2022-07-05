open Core
module Ir = Ir_
module IdMap = Symbol.Map
module IdSet = Symbol.Set

module Context : sig
  type t

  val empty : t
  val add_static : t -> Ir.static -> t
  val add_func : t -> Ir.id -> Ir.func -> t
  val get_func : t -> Ir.id -> Ir.func
end = struct
  type t =
    { statics : Ir.static list
    ; funcs : Ir.func IdMap.t
    }

  let empty = { statics = []; funcs = IdMap.empty }
  let add_static ctx data = { ctx with statics = ctx.statics @ [ data ] }
  let add_func ctx key data = { ctx with funcs = IdMap.add_exn ctx.funcs ~key ~data }
  let get_func ctx key = IdMap.find_exn ctx.funcs key
end

let materialize args =
  let ss, is =
    List.map args ~f:(function
        | Ir.Vvar v -> None, v
        | v ->
          let z = Symbol.nonce () in
          Some (Ir.Sassign (z, Eval v)), z)
    |> List.unzip
  in
  Ir.Sseq (List.filter_opt ss), is
;;

let rename_id m x =
  match IdMap.find m x with
  | Some y -> y
  | None -> x
;;

let rec rename_val m = function
  | Ir.Vvar x -> Ir.Vvar (rename_id m x)
  | Vprod vs -> Vprod (List.map ~f:(rename_val m) vs)
  | Vunit -> Vunit
  | Vnum i -> Vnum i
  | Vbool b -> Vbool b
  | Vnull -> Vnull
  | Vptr _ -> assert false
;;

let rename_exp m = function
  | Ir.Eval v -> Ir.Eval (rename_val m v)
  | Eproj (x, i) -> Eproj (rename_id m x, i)
  | Euop (uop, x) -> Euop (uop, rename_id m x)
  | Ebop (bop, x, y) -> Ebop (bop, rename_id m x, rename_id m y)
  | Ealloc t -> Ealloc t
  | Efun c ->
    let args = List.map ~f:(rename_val m) c.args in
    Efun { c with args }
;;

let rec rename_stmt m = function
  | Ir.Sseq ss -> List.map ~f:(rename_stmt m) ss |> Ir.Sseq
  | Sassign (x, e) -> Sassign (rename_id m x, rename_exp m e)
  | Sunassign (x, e) -> Sunassign (rename_id m x, rename_exp m e)
  | Sswap (x, y) -> Sswap (rename_id m x, rename_id m y)
  | Smem_swap (x, y) -> Smem_swap (rename_id m x, rename_id m y)
  | Sif (x, s1, s2) -> Sif (rename_id m x, rename_stmt m s1, rename_stmt m s2)
  | Sdebug -> Sdebug
;;

let rec locals = function
  | Ir.Sseq ss -> List.map ~f:locals ss |> List.fold ~init:IdSet.empty ~f:IdSet.union
  | Sassign (x, _) -> IdSet.singleton x
  | Sunassign (x, _) -> IdSet.singleton x
  | Sswap _ -> IdSet.empty
  | Smem_swap _ -> IdSet.empty
  | Sif (_, s1, s2) -> IdSet.union (locals s1) (locals s2)
  | Sdebug -> IdSet.empty
;;

let rename_func args x (f : Ir.func) : Ir.func =
  let mappings =
    List.fold2_exn
      (List.map f.args ~f:(fun x -> x.name))
      args
      ~init:IdMap.empty
      ~f:(fun acc key data -> IdMap.add_exn acc ~key ~data)
    |> IdMap.add_exn ~key:f.result.name ~data:x
  in
  let mappings =
    IdSet.fold
      ~init:mappings
      ~f:(fun acc key ->
        match IdMap.add acc ~key ~data:(Symbol.nonce ()) with
        | `Ok m -> m
        | _ -> acc)
      (locals f.body)
  in
  let body = rename_stmt mappings f.body in
  let args =
    List.map2_exn f.args args ~f:(fun x name : Ir.id_typ -> { name; typ = x.typ })
  in
  let result = { f.result with name = x } in
  { f with args; result; body }
;;

let default_body (f : Ir.func) =
  Ir.Sassign (f.result.name, Eval (Ir_lower.default f.result.typ))
;;

let rec inline_call ctx bound x (c : Ir.call) =
  let f = Context.get_func ctx c.name in
  let s, args = materialize c.args in
  let f = rename_func args x f in
  let bound =
    match bound, c.bound, f.bound with
    | Some (n, v1), Some (Ovar v2), Some (Ovar v) when Symbol.equal v1 v2 -> Some (n, v)
    | Some (n, v1), Some (Ominus (v2, i)), Some (Ovar v) when Symbol.equal v1 v2 ->
      Some (n - i, v)
    | _, Some (Oconst i), Some (Ovar v) -> Some (i, v)
    | _, None, None -> None
    | _ -> assert false
  in
  Ir.Sseq [ s; inline_func ctx bound f; Ir_lower.reverse s ]

and inline_stmt ctx bound = function
  | Ir.Sseq ss -> List.map ~f:(inline_stmt ctx bound) ss |> Ir.Sseq
  | Sswap (x, y) -> Sswap (x, y)
  | Smem_swap (x, y) -> Smem_swap (x, y)
  | Sif (x, s1, s2) -> Sif (x, inline_stmt ctx bound s1, inline_stmt ctx bound s2)
  | Sdebug -> Sdebug
  | Sassign (x, Efun c) -> inline_call ctx bound x c
  | Sassign (x, e) -> Sassign (x, e)
  | Sunassign (x, Efun c) -> inline_call ctx bound x c |> Ir_lower.reverse
  | Sunassign (x, e) -> Sunassign (x, e)

and inline_func ctx bound (f : Ir.func) =
  match bound with
  | Some (0, _) -> default_body f
  | _ ->
    let body = inline_stmt ctx bound f.body in
    body
;;

let inline ?(bounds = IdMap.empty) (decls : Ir.decl list) =
  List.folding_map decls ~init:Context.empty ~f:(fun ctx -> function
    | Dstatic s -> Context.add_static ctx s, None
    | Dtype _ -> ctx, None
    | Dfunc f ->
      let ctx = Context.add_func ctx f.name f in
      let func =
        match f.bound, IdMap.find bounds f.name with
        | Some (Ovar v), Some bound ->
          Some { f with body = inline_func ctx (Some (bound, v)) f }
        | None, None -> Some { f with body = inline_func ctx None f }
        | None, Some _ -> failwith ("Unexpected bound for function " ^ Symbol.name f.name)
        | Some _, None -> None
        | _, _ -> assert false
      in
      ctx, func)
  |> List.filter_opt
;;
