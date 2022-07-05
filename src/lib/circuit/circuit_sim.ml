open Core
module WireTable = Int.Table
module Circuit = Circuit_

exception OutOfMemory
exception NullDereference

module State = struct
  type t =
    { reg : bool WireTable.t
    ; mem : int array
    ; hp : int array
    }

  let init ~mem_size =
    { reg = WireTable.create ()
    ; mem = Array.init mem_size ~f:(fun i -> if i = mem_size - 1 then 0 else i + 1)
    ; hp = Array.init !Args.word_size ~f:(fun _ -> 0)
    }
  ;;

  let show_reg state =
    WireTable.to_alist state.reg
    |> List.sort ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2)
    |> List.map ~f:(fun (i, v) -> Format.sprintf "%d -> %s" i (Bool.to_string v))
    |> String.concat ?sep:(Some ", ")
    |> fun s -> "{ " ^ s ^ " }"
  ;;

  let show_mem state =
    Array.to_list state.mem
    |> List.map ~f:Int.to_string
    |> String.concat ?sep:(Some "; ")
    |> fun s -> "[ " ^ s ^ " ]"
  ;;

  let debug state =
    Format.eprintf
      "*** DEBUG ***\nRegister: %s\nMemory: %s\n%!"
      (show_reg state)
      (show_mem state)
  ;;

  let get_reg state = WireTable.find_exn state.reg
  let has_reg state = WireTable.mem state.reg
  let is_unset state w = not (get_reg state w)

  let set_reg state key data =
    WireTable.remove state.reg key;
    WireTable.add_exn state.reg ~key ~data
  ;;

  let del_reg state key = WireTable.remove state.reg key
  let get_mem state p = state.mem.(p)
  let set_mem state p v = state.mem.(p) <- v

  let get_word state ws =
    List.map ws ~f:(get_reg state)
    |> List.map ~f:(fun x -> if x then "1" else "0")
    |> String.concat ~sep:""
    |> String.rev
    |> fun s -> "0b" ^ s |> Int.of_string
  ;;

  let set_word state ws n =
    let bits = List.init (List.length ws) ~f:(fun i -> n land (1 lsl i) <> 0) in
    List.iter2_exn ws bits ~f:(fun w b -> set_reg state w b)
  ;;

  let alloc state hp =
    let p = get_word state hp in
    let v = state.mem.(p) in
    if v = 0
    then raise OutOfMemory
    else (
      state.mem.(p) <- 0;
      set_word state hp v;
      p)
  ;;

  let dealloc state hp p =
    state.mem.(p) <- get_word state hp;
    set_word state hp p
  ;;
end

let try_dealloc state x ws t =
  let p = State.get_word state ws in
  let v = State.get_mem state p in
  if v <> 0
  then
    raise
      (Errors.RuntimeError
         ( Format.sprintf
             "Could not deallocate variable '%s' using expression '%s'; expected default \
              value, heap contains '%x'"
             (Symbol.name x)
             (Lir_pp.show_exp (Ealloc t))
             v
         , fun () -> State.debug state ));
  State.del_reg state x;
  State.dealloc state ws
;;

let check_regs_cleared (state : State.t) out_arg =
  let remaining = state.reg |> WireTable.keys |> Int.Set.of_list in
  let out_arg = out_arg |> Int.Set.of_list in
  assert (
    Int.Set.is_empty
    @@ Int.Set.diff (Int.Set.diff remaining out_arg) (Int.Set.of_array state.hp))
;;

let rec exec_gate (state : State.t) fwd s =
  match s with
  | Circuit.Gnum (n, ws) ->
    if fwd
    then (
      assert (List.for_all ws ~f:(State.is_unset state));
      State.set_word state ws n)
    else (
      let n' = State.get_word state ws in
      assert (n = n');
      State.set_word state ws 0)
  | Gbool (b, w) ->
    if fwd
    then (
      assert (State.is_unset state w);
      State.set_reg state w b)
    else (
      let b' = State.get_reg state w in
      assert (Bool.(b = b'));
      State.set_reg state w false)
  | Gnot ws ->
    List.iter ws ~f:(fun w -> State.set_reg state w @@ not @@ State.get_reg state w)
  | Gcopy (ws1, ws2) ->
    if fwd
    then (
      assert (List.for_all ws2 ~f:(State.is_unset state));
      List.iter2_exn ws1 ws2 ~f:(fun w1 w2 ->
          State.set_reg state w2 @@ State.get_reg state w1))
    else (
      List.iter2_exn ws1 ws2 ~f:(fun w1 w2 ->
          assert (Bool.(State.get_reg state w1 = State.get_reg state w2)));
      List.iter ws2 ~f:(fun w2 -> State.set_reg state w2 false))
  | Gbop (bop, ws1, ws2, ws3) ->
    let x = State.get_word state ws1 in
    let y = State.get_word state ws2 in
    let n = 1 lsl !Args.word_size in
    let res =
      match bop with
      | Bland -> x land y
      | Blor -> x lor y
      | Beq -> if x = y then 1 else 0
      | Blsl -> (x lsl y) mod n
      | Blsr -> x lsr y
      | Bplus -> (x + y) mod n
      | Bminus -> (x - y + n) mod n
      | Btimes -> x * y mod n
      | Bless -> if x < y then 1 else 0
    in
    if fwd
    then (
      assert (List.for_all ws3 ~f:(State.is_unset state));
      State.set_word state ws3 res)
    else (
      assert (State.get_word state ws3 = res);
      State.set_word state ws3 0)
  | Gswap (ws1, ws2) ->
    List.iter2_exn ws1 ws2 ~f:(fun w1 w2 ->
        let b = State.get_reg state w1 in
        State.set_reg state w1 (State.get_reg state w2);
        State.set_reg state w2 b)
  | Gmem_swap (ws1, ws2) ->
    let p = State.get_word state ws1 in
    if p = 0
    then raise NullDereference
    else (
      let v = State.get_mem state p in
      State.set_mem state p (State.get_word state ws2);
      State.set_word state ws2 v)
  | Gcond (w, gs) ->
    if State.get_reg state w then List.iter gs ~f:(exec_gate state fwd) else ()
  | Gconj gs -> List.rev gs |> List.iter ~f:(exec_gate state (not fwd))
  | Ginit w ->
    if fwd
    then (
      assert (not @@ State.has_reg state w);
      State.set_reg state w false)
    else (
      assert (State.is_unset state w);
      State.del_reg state w)
;;

let check_leaks (state : State.t) mem_size =
  let initial = State.init ~mem_size in
  let initial_mem = Int.Set.add (Int.Set.of_array initial.mem) 1 in
  let final_mem =
    Int.Set.union (Int.Set.of_array state.mem) @@ Int.Set.of_array state.hp
  in
  Int.Set.equal initial_mem final_mem && Array.mem ~equal:Int.equal state.mem 0
;;

let interp ~mem_size (modules : Circuit.modul list) =
  let state = State.init ~mem_size in
  List.iter modules ~f:(fun m ->
      if m.name = Symbol.get_sym "main"
      then (
        assert (List.is_empty m.args);
        List.iteri m.hp ~f:(fun i w -> state.hp.(i) <- w);
        State.set_word state m.hp 1;
        List.iter m.body ~f:(exec_gate state true);
        check_regs_cleared state m.out_args;
        if not (check_leaks state mem_size)
        then
          Format.eprintf
            "Warning: memory may not have been reset to initial state at program \
             termination.\n"
        else Format.eprintf "Exited normally.\n"));
  state
;;
