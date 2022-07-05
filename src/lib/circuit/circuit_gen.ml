open Core
module IdMap = Symbol.Map
module Circuit = Circuit_

module Context : sig
  type t

  val empty : hp:Circuit.wire list -> t
  val get_reg : t -> Symbol.t -> Circuit.wire list
  val add_reg : t -> Symbol.t -> Circuit.wire list -> t
  val remove_reg : t -> Symbol.t -> t
  val hp : t -> Circuit.wire list
end = struct
  type t =
    { regs : Circuit.wire list IdMap.t
    ; hp : Circuit.wire list
    }

  let empty ~hp : t = { regs = IdMap.empty; hp }
  let get_reg ctx key = IdMap.find_exn ctx.regs key
  let add_reg ctx key data = { ctx with regs = IdMap.add_exn ctx.regs ~key ~data }
  let remove_reg ctx key = { ctx with regs = IdMap.remove ctx.regs key }
  let hp ctx = ctx.hp
end

let init_wires = List.map ~f:Circuit.ginit
let dealloc_wires a = List.iter ~f:(Alloc.dealloc a)

let rec type_size = function
  | Lir.Tunit -> 0
  | Tbool -> 1
  | Tuint -> !Args.word_size
  | Tprod ts -> List.fold ~init:0 ~f:( + ) (List.map ~f:type_size ts)
;;

let rec value_size ctx = function
  | Lir.Vvar x -> List.length (Context.get_reg ctx x)
  | Vunit -> 0
  | Vnum _ -> !Args.word_size
  | Vbool _ -> 1
  | Vprod vs -> List.map ~f:(value_size ctx) vs |> List.fold ~init:0 ~f:( + )
;;

let rec lower_value ctx ws q = function
  | Lir.Vvar x -> Queue.enqueue q @@ Circuit.Gcopy (Context.get_reg ctx x, ws)
  | Vunit -> ()
  | Vnum i -> Queue.enqueue q @@ Gnum (i, ws)
  | Vbool b -> Queue.enqueue q @@ Gbool (b, List.hd_exn ws)
  | Vprod vs ->
    List.iteri vs ~f:(fun len v ->
        let pos = value_size ctx (Lir.Vprod (List.sub vs ~pos:0 ~len)) in
        lower_value ctx (List.sub ws ~pos ~len:(value_size ctx v)) q v)
;;

let lower_exp ctx ws q = function
  | Lir.Eval v -> lower_value ctx ws q v
  | Eproj (x, p, i) ->
    let x = Context.get_reg ctx x in
    let pos = type_size @@ Lir.Tprod (List.sub p ~pos:0 ~len:i) in
    let len = type_size @@ List.nth_exn p i in
    Queue.enqueue q @@ Gcopy (List.sub ~pos ~len x, ws)
  | Euop (Ulnot, x) ->
    let x = Context.get_reg ctx x in
    Queue.enqueue_all q @@ [ Gcopy (x, ws); Gnot ws ]
  | Ebop (bop, x, y) ->
    let x = Context.get_reg ctx x in
    let y = Context.get_reg ctx y in
    Queue.enqueue q @@ Gbop (bop, x, y, ws)
  | Ealloc _ ->
    let hp = Context.hp ctx in
    Queue.enqueue_all q @@ [ Gswap (ws, hp); Gmem_swap (ws, hp) ]
;;

let rec lower_stmt ctx a q = function
  | Lir.Sassign (t, x, e) ->
    let sz = type_size t in
    let ws = Alloc.alloc_n a sz in
    Queue.enqueue_all q @@ init_wires ws;
    lower_exp ctx ws q e;
    Context.add_reg ctx x ws
  | Sunassign (_, x, e) ->
    let ws = Context.get_reg ctx x in
    dealloc_wires a ws;
    let q' = Queue.create () in
    Queue.enqueue_all q' @@ init_wires ws;
    lower_exp ctx ws q' e;
    Queue.enqueue q @@ Gconj (Queue.to_list q');
    Context.remove_reg ctx x
  | Sswap (x, y) ->
    let ws1 = Context.get_reg ctx x in
    let ws2 = Context.get_reg ctx y in
    (* TODO: can this sometimes be a logical swap? *)
    Queue.enqueue q @@ Gswap (ws1, ws2);
    ctx
  | Smem_swap (x, y) ->
    let ws1 = Context.get_reg ctx x in
    let ws2 = Context.get_reg ctx y in
    Queue.enqueue q @@ Gmem_swap (ws1, ws2);
    ctx
  | Sif (x, ss) ->
    let q' = Queue.create () in
    let ctx = List.fold ~init:ctx ~f:(fun ctx s -> lower_stmt ctx a q' s) ss in
    let w = Context.get_reg ctx x |> List.hd_exn in
    Queue.enqueue q @@ Gcond (w, Queue.to_list q');
    ctx
;;

let rec max_alloc_size =
  List.fold ~init:!Args.word_size ~f:(fun acc -> function
    | Lir.Sassign (t, _, _) -> Int.max (type_size t) acc
    | Sunassign (t, _, _) -> Int.max (type_size t) acc
    | Sif (_, ss) -> Int.max acc (max_alloc_size ss)
    | Sswap _ | Smem_swap _ -> acc)
;;

let lower_module ~word_size (m : Lir.modul) : Circuit.modul =
  let a = Alloc.empty () in
  let hp = Alloc.alloc_n a word_size in
  let ctx = Context.empty ~hp in
  let _, out_arg = m.out_arg in
  let ctx, args =
    List.fold m.args ~init:(ctx, []) ~f:(fun (ctx, acc) (t, arg) ->
        let arg_sz = type_size t in
        let arg_wires = Alloc.alloc_n a arg_sz in
        Context.add_reg ctx arg arg_wires, acc @ arg_wires)
  in
  let q = Queue.create () in
  let ctx = List.fold m.body ~init:ctx ~f:(fun ctx s -> lower_stmt ctx a q s) in
  let body = Queue.to_list q in
  let out_args = Context.get_reg ctx out_arg in
  let cell_size = max_alloc_size m.body in
  { name = m.name; hp; out_args; args; body; cell_size }
;;
