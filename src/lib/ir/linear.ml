open Core
module Ir = Ir_
module IdMap = Symbol.Map
module IdSet = Symbol.Set

module Context = struct
  type t =
    { marked : IdSet.t
    ; map : Symbol.t IdMap.t
    }

  let empty : t = { marked = IdSet.empty; map = IdMap.empty }

  let map ctx v =
    match IdMap.find ctx.map v with
    | Some v -> v
    | None -> v
  ;;

  let set_map ctx key data = { ctx with map = IdMap.add_exn ctx.map ~key ~data }
  let marked ctx = IdSet.mem ctx.marked
  let mark ctx x = { ctx with marked = IdSet.add ctx.marked x }
end

let rec resolve_value ctx = function
  | Ir.Vvar v -> Ir.Vvar (Context.map ctx v)
  | Vunit -> Vunit
  | Vnum i -> Vnum i
  | Vbool b -> Vbool b
  | Vnull -> Vnull
  | Vptr (t, p) -> Vptr (t, p)
  | Vprod vs -> Vprod (List.map ~f:(resolve_value ctx) vs)
;;

let resolve_exp ctx = function
  | Ir.Eval v -> Ir.Eval (resolve_value ctx v)
  | Eproj (x, i) -> Eproj (Context.map ctx x, i)
  | Euop (uop, x) -> Euop (uop, Context.map ctx x)
  | Ebop (bop, x, y) -> Ebop (bop, Context.map ctx x, Context.map ctx y)
  | Ealloc t -> Ealloc t
  | Efun _ -> assert false
;;

let rec replace_rem vars ctx = function
  | Ir.Sunassign (x, e) when IdSet.mem vars x ->
    let e = resolve_exp ctx e in
    if not (Context.marked ctx x)
    then (
      let y = Symbol.nonce () in
      let ctx = Context.mark ctx x in
      Context.set_map ctx x y, Ir.Sunassign (x, e))
    else ctx, Sunassign (Context.map ctx x, e)
  | Sunassign (x, e) -> ctx, Sunassign (x, resolve_exp ctx e)
  | Sassign (x, e) when IdSet.mem vars x ->
    ctx, Sassign (Context.map ctx x, resolve_exp ctx e)
  | Sassign (x, e) -> ctx, Sassign (x, resolve_exp ctx e)
  | Sswap (x, y) -> ctx, Sswap (Context.map ctx x, Context.map ctx y)
  | Smem_swap (x, y) -> ctx, Smem_swap (Context.map ctx x, Context.map ctx y)
  | Sdebug -> ctx, Sdebug
  | Sif (x, s1, s2) ->
    let _, s1 = replace_rem vars ctx s1 in
    let ctx, s2 = replace_rem vars ctx s2 in
    ctx, Sif (Context.map ctx x, s1, s2)
  | Sseq ss ->
    let ctx, ss = List.fold_map ~init:ctx ~f:(replace_rem vars) ss in
    ctx, Sseq ss
;;

let replace_rem vars s = snd @@ replace_rem vars Context.empty s

let rec net_bound (add, rem) = function
  | Ir.Sassign (key, _) ->
    if IdSet.mem rem key then add, IdSet.remove rem key else IdSet.add add key, rem
  | Sunassign (key, _) ->
    if IdSet.mem add key then IdSet.remove add key, rem else add, IdSet.add rem key
  | Sswap _ | Smem_swap _ | Sdebug -> add, rem
  | Sseq ss -> List.fold ~init:(add, rem) ~f:net_bound ss
  | Sif (_, s1, s2) ->
    let add1, rem1 = net_bound (add, rem) s1 in
    let add2, rem2 = net_bound (add, rem) s2 in
    assert (IdSet.equal add1 add2 && IdSet.equal rem1 rem2);
    add1, rem1
;;

let net_bound = net_bound (IdSet.empty, IdSet.empty)

let rec linearize_stmt s =
  match s with
  | Ir.Sif (x, s1, s2) ->
    let s1 = linearize_stmt s1 in
    let s2 = linearize_stmt s2 in
    let add1, rem1 = net_bound s1 in
    let add2, rem2 = net_bound s2 in
    assert (IdSet.equal add1 add2 && IdSet.equal rem1 rem2);
    Ir.Sif
      ( x
      , replace_rem rem1 @@ Ir_lower.reverse @@ replace_rem add1 @@ Ir_lower.reverse s1
      , replace_rem rem2 @@ Ir_lower.reverse @@ replace_rem add2 @@ Ir_lower.reverse s2 )
  | Sseq ss -> Sseq (List.map ~f:linearize_stmt ss)
  | Sassign _ | Sunassign _ | Sswap _ | Smem_swap _ | Sdebug -> s
;;

let linearize_func (f : Ir.func) =
  let body = linearize_stmt f.body in
  { f with body }
;;

let linearize = List.map ~f:linearize_func
