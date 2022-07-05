open Core
module IdTable = Symbol.Table
module Lir = Lir_

exception OutOfMemory
exception NullDereference

module State = struct
  type t =
    { reg : Lir.value IdTable.t
    ; mem : Lir.value array
    ; heap : int ref
    }

  let init ~mem_size =
    { reg = IdTable.create ()
    ; mem =
        Array.init mem_size ~f:(fun i ->
            if i = mem_size - 1 then Lir.Vnum 0 else Vnum (i + 1))
    ; heap = ref 1
    }
  ;;

  let show_reg state =
    IdTable.to_alist state.reg
    |> List.sort ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2)
    |> List.map ~f:(fun (i, v) ->
           Format.sprintf "'%s' -> '%s'" (Symbol.name i) (Lir_pp.show_value v))
    |> String.concat ?sep:(Some ", ")
    |> fun s -> "{ " ^ s ^ " }"
  ;;

  let show_mem state =
    Array.to_list state.mem
    |> List.map ~f:Lir_pp.show_value
    |> String.concat ?sep:(Some "; ")
    |> fun s -> "[ " ^ s ^ " ]"
  ;;

  let debug state =
    Format.eprintf
      "*** DEBUG ***\nRegister: %s\nHeap: %s\nMemory: %s\n%!"
      (show_reg state)
      (Int.to_string !(state.heap))
      (show_mem state)
  ;;

  let get_reg state = IdTable.find_exn state.reg

  let set_reg state key data =
    IdTable.remove state.reg key;
    IdTable.add_exn state.reg ~key ~data
  ;;

  let del_reg state key = IdTable.remove state.reg key
  let get_mem state p = state.mem.(p)
  let set_mem state p v = state.mem.(p) <- v

  let alloc state t =
    let p = !(state.heap) in
    let v = state.mem.(p) in
    state.mem.(p) <- Lir_lower.default t;
    (match v with
    | Vnum 0 -> raise OutOfMemory
    | Vnum v -> state.heap := v
    | _ -> assert false);
    Lir.Vnum p
  ;;

  let dealloc state p =
    state.mem.(p) <- Vnum !(state.heap);
    state.heap := p
  ;;
end

let rec resolve_value (state : State.t) = function
  | Lir.Vvar v -> State.get_reg state v
  | Vprod vs -> Vprod (List.map ~f:(resolve_value state) vs)
  | v -> v
;;

let eval_exp (state : State.t) e =
  match e with
  | Lir.Eval v -> resolve_value state v
  | Eproj (x, _, i) ->
    (match State.get_reg state x with
    | Vprod vs -> resolve_value state @@ List.nth_exn vs i
    | _ -> assert false)
  | Euop (uop, x) ->
    (match uop, State.get_reg state x with
    | Ulnot, Vbool b -> Vbool (not b)
    | Ulnot, Vnum n -> Vnum (lnot n)
    | _ -> assert false)
  | Ebop (bop, x1, x2) ->
    (match bop, State.get_reg state x1, State.get_reg state x2 with
    | Bland, Vbool b1, Vbool b2 -> Vbool (b1 && b2)
    | Blor, Vbool b1, Vbool b2 -> Vbool (b1 || b2)
    | Bplus, Vnum n1, Vnum n2 -> Vnum (n1 + n2)
    | Bminus, Vnum n1, Vnum n2 -> Vnum (n1 - n2)
    | Btimes, Vnum n1, Vnum n2 -> Vnum (n1 * n2)
    | Beq, Vbool b1, Vbool b2 -> Vbool Bool.(b1 = b2)
    | Beq, Vnum n1, Vnum n2 -> Vbool (n1 = n2)
    | Bless, Vnum n1, Vnum n2 -> Vbool (n1 < n2)
    | Bless, Vbool b1, Vbool b2 -> Vbool Bool.(b1 < b2)
    | Bland, Vnum n1, Vnum n2 -> Vnum (n1 land n2)
    | Blor, Vnum n1, Vnum n2 -> Vnum (n1 lor n2)
    | Blsl, Vnum n1, Vnum n2 -> Vnum (n1 lsl n2)
    | Blsr, Vnum n1, Vnum n2 -> Vnum (n1 lsr n2)
    | _ -> assert false)
  | Ealloc t -> State.alloc state t
;;

let try_dealloc state x p t =
  let v = State.get_mem state p in
  if not (Lir.equal_value v (Lir_lower.default t))
  then
    raise
      (Errors.RuntimeError
         ( Format.sprintf
             "Could not deallocate variable '%s' using expression '%s'; expected default \
              value, heap contains '%s'"
             (Symbol.name x)
             (Lir_pp.show_exp (Ealloc t))
             (Lir_pp.show_value v)
         , fun () -> State.debug state ));
  State.del_reg state x;
  State.dealloc state p
;;

let check_regs_cleared (state : State.t) out_arg =
  let remaining = state.reg |> IdTable.keys |> Symbol.Set.of_list in
  assert (Symbol.Set.is_empty @@ Symbol.Set.remove remaining out_arg)
;;

let rec exec_stmt (state : State.t) s =
  match s with
  | Lir.Sassign (_, x, e) -> State.set_reg state x (eval_exp state e)
  | Sunassign (_, x, Ealloc sz) ->
    (match State.get_reg state x with
    | Vnum p -> try_dealloc state x p sz
    | _ -> assert false)
  | Sunassign (_, x, e) ->
    let v = State.get_reg state x in
    let v' = eval_exp state e in
    if not (Lir.equal_value v v')
    then
      raise
        (Errors.RuntimeError
           ( Format.sprintf
               "Could not uncompute variable '%s' using expression '%s'; expected value \
                '%s', got value '%s'"
               (Symbol.name x)
               (Lir_pp.show_exp e)
               (Lir_pp.show_value v)
               (Lir_pp.show_value v')
           , fun () -> State.debug state ));
    State.del_reg state x
  | Sswap (x1, x2) ->
    let v = State.get_reg state x2 in
    State.set_reg state x2 (State.get_reg state x1);
    State.set_reg state x1 v
  | Smem_swap (p, x) ->
    let v = State.get_reg state p in
    (match v with
    | Vnum p when p <> 0 ->
      let v = State.get_mem state p in
      let v' = State.get_reg state x in
      State.set_mem state p v';
      State.set_reg state x v
    | _ -> raise NullDereference)
  | Sif (x, ss) ->
    let v = State.get_reg state x in
    (match v with
    | Vbool true -> List.iter ss ~f:(exec_stmt state)
    | Vbool false -> ()
    | _ -> assert false)
;;

let check_leaks (state : State.t) mem_size =
  let initial = State.init ~mem_size in
  let to_set m =
    m
    |> Array.filter_map ~f:(function
           | Lir.Vnum i -> Some i
           | _ -> None)
    |> Int.Set.of_array
  in
  let initial_mem = Int.Set.add (to_set initial.mem) !(initial.heap) in
  let final_mem = Int.Set.add (to_set state.mem) !(state.heap) in
  Int.Set.equal initial_mem final_mem
  && Array.mem ~equal:Lir.equal_value state.mem (Vnum 0)
;;

let interp ~mem_size (modules : Lir.modul list) =
  let state = State.init ~mem_size in
  List.iter modules ~f:(fun m ->
      if m.name = Symbol.get_sym "main"
      then (
        assert (List.is_empty m.args);
        List.iter m.body ~f:(exec_stmt state);
        check_regs_cleared state (snd m.out_arg);
        if not (check_leaks state mem_size)
        then
          Format.eprintf
            "Warning: memory may not have been reset to initial state at program \
             termination.\n"
        else Format.eprintf "Exited normally.\n"));
  state
;;
