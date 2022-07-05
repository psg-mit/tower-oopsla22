open Core
module IdTable = Symbol.Table

exception OutOfMemory
exception NullDereference

module State = struct
  type t =
    { reg : Ir.value IdTable.t
    ; mem : Ir.value array
    ; mutable static : Ir.value array
    ; heap : int ref
    ; mem_size : int
    ; bounds : int IdTable.t
    ; funcs : Ir.func IdTable.t
    ; ctx : Ast.Check.Context.t
    ; frame : (bool * Ir.id) list
    ; word_size : int
    }

  let init ~mem_size ctx =
    { reg = IdTable.create ()
    ; mem =
        Array.init mem_size ~f:(fun i ->
            if i = mem_size - 1 then Ir.Vnull else Vptr (Tunit, i + 1))
    ; static = Array.create ~len:0 (Ir.Vnum 0)
    ; heap = ref 0
    ; mem_size
    ; bounds = IdTable.create ()
    ; funcs = IdTable.create ()
    ; ctx
    ; frame = []
    ; word_size = !Args.word_size
    }
  ;;

  let show_reg state =
    IdTable.to_alist state.reg
    |> List.sort ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2)
    |> List.map ~f:(fun (i, v) ->
           Format.sprintf "'%s' -> '%s'" (Symbol.name i) (Ir.Pp.show_value v))
    |> String.concat ?sep:(Some ", ")
    |> fun s -> "{ " ^ s ^ " }"
  ;;

  let show_mem state =
    Array.to_list state.mem
    |> List.map ~f:Ir.Pp.show_value
    |> String.concat ?sep:(Some "; ")
    |> fun s -> "[ " ^ s ^ " ]"
  ;;

  let check_leaks state =
    let initial = init ~mem_size:state.mem_size state.ctx in
    let to_set m =
      m
      |> Array.filter_map ~f:(function
             | Ir.Vptr (Tunit, i) -> Some i
             | _ -> None)
      |> Int.Set.of_array
    in
    let initial_mem = Int.Set.add (to_set initial.mem) !(initial.heap) in
    let final_mem = Int.Set.add (to_set state.mem) !(state.heap) in
    Int.Set.equal initial_mem final_mem && Array.mem ~equal:Ir.equal_value state.mem Vnull
  ;;

  let show_static state =
    Array.to_list state.static
    |> List.map ~f:Ir.Pp.show_value
    |> String.concat ?sep:(Some "; ")
    |> fun s -> "[ " ^ s ^ " ]"
  ;;

  let print_backtrace state =
    Format.eprintf "\nBacktrace (most recent call first):\n";
    List.iter state.frame ~f:(fun (forward, id) ->
        Format.eprintf
          "  %s%s\n"
          (Symbol.name id)
          (if not forward then " (backwards)" else ""))
  ;;

  let debug state =
    Format.eprintf
      "*** DEBUG ***\nRegister: %s\nHeap: %s\nMemory: %s\nStatic: %s\n%!"
      (show_reg state)
      (Int.to_string !(state.heap))
      (show_mem state)
      (show_static state)
  ;;

  let dump state () =
    debug state;
    print_backtrace state
  ;;

  let get_reg state = IdTable.find_exn state.reg

  let set_reg state key data =
    IdTable.remove state.reg key;
    IdTable.add_exn state.reg ~key ~data
  ;;

  let del_reg state key = IdTable.remove state.reg key

  let get_mem state p =
    if p < state.mem_size then state.mem.(p) else state.static.(p - state.mem_size)
  ;;

  let set_mem state p v =
    if p < state.mem_size
    then state.mem.(p) <- v
    else state.static.(p - state.mem_size) <- v
  ;;

  let get_bound state = IdTable.find state.bounds
  let set_bound state key data = IdTable.add_exn state.bounds ~key ~data

  let alloc state typ =
    let p = !(state.heap) in
    let v = state.mem.(p) in
    state.mem.(p) <- Ir.Lower.default (Ast.Check.normalize_type state.ctx typ);
    (match v with
    | Vptr (_, v) -> state.heap := v
    | _ -> raise OutOfMemory);
    Ir.Vptr (typ, p)
  ;;

  let alloc_static state typ values =
    let a = List.to_array values in
    let p = Array.length state.static in
    state.static <- Array.append state.static a;
    Ir.Vptr (typ, p + state.mem_size)
  ;;

  let dealloc state p =
    state.mem.(p) <- Vptr (Tunit, !(state.heap));
    state.heap := p
  ;;
end

let rec resolve_value (state : State.t) = function
  | Ir.Vvar v -> State.get_reg state v
  | Vprod vs -> Vprod (List.map ~f:(resolve_value state) vs)
  | v -> v
;;

let eval_exp (state : State.t) e =
  match e with
  | Ir.Eval v -> resolve_value state v
  | Eproj (x, i) ->
    (match State.get_reg state x with
    | Vprod vs -> List.nth_exn vs i
    | _ -> assert false)
  | Euop (uop, x) ->
    (match uop, State.get_reg state x with
    | Unot, Vbool b -> Vbool (not b)
    | Utest, Vnum n -> Vbool (n = 0)
    | Utest, Vptr _ -> Vbool false
    | Utest, Vnull -> Vbool true
    | Ulnot, Vnum n -> Vnum (lnot n)
    | _ -> assert false)
  | Ebop (bop, x1, x2) ->
    (match bop, State.get_reg state x1, State.get_reg state x2 with
    | Band, Vbool b1, Vbool b2 -> Vbool (b1 && b2)
    | Bor, Vbool b1, Vbool b2 -> Vbool (b1 || b2)
    | Bplus, Vnum n1, Vnum n2 -> Vnum (n1 + n2)
    | Bminus, Vnum n1, Vnum n2 -> Vnum (n1 - n2)
    | Btimes, Vnum n1, Vnum n2 -> Vnum (n1 * n2)
    | Beq, Vbool b1, Vbool b2 -> Vbool Bool.(b1 = b2)
    | Beq, Vnum n1, Vnum n2 -> Vbool (n1 = n2)
    | Beq, Vptr (_, p1), Vptr (_, p2) -> Vbool (p1 = p2)
    | Beq, Vptr _, Vnull | Beq, Vnull, Vptr _ -> Vbool false
    | Beq, Vnull, Vnull -> Vbool true
    | Bneq, Vbool b1, Vbool b2 -> Vbool Bool.(b1 <> b2)
    | Bneq, Vnum n1, Vnum n2 -> Vbool (n1 <> n2)
    | Bneq, Vptr (_, p1), Vptr (_, p2) -> Vbool (p1 <> p2)
    | Bneq, Vptr _, Vnull | Bneq, Vnull, Vptr _ -> Vbool true
    | Bneq, Vnull, Vnull -> Vbool false
    | Bplus, Vptr (t, p), Vnum n -> Vptr (t, p + n)
    | Bminus, Vptr (t, p), Vnum n -> Vptr (t, p - n)
    | Bless, Vnum n1, Vnum n2 -> Vbool (n1 < n2)
    | Bgreater, Vnum n1, Vnum n2 -> Vbool (n1 > n2)
    | Ble, Vnum n1, Vnum n2 -> Vbool (n1 <= n2)
    | Bge, Vnum n1, Vnum n2 -> Vbool (n1 >= n2)
    | Bless, Vbool b1, Vbool b2 -> Vbool Bool.(b1 < b2)
    | Bgreater, Vbool b1, Vbool b2 -> Vbool Bool.(b1 > b2)
    | Ble, Vbool b1, Vbool b2 -> Vbool Bool.(b1 <= b2)
    | Bge, Vbool b1, Vbool b2 -> Vbool Bool.(b1 >= b2)
    | Bland, Vnum n1, Vnum n2 -> Vnum (n1 land n2)
    | Blor, Vnum n1, Vnum n2 -> Vnum (n1 lor n2)
    | Blsl, Vnum n1, Vnum n2 ->
      if n2 >= state.word_size
      then Errors.word_size_error n2 state.word_size (State.dump state);
      Vnum (n1 lsl n2)
    | Blsr, Vnum n1, Vnum n2 ->
      if n2 >= state.word_size
      then Errors.word_size_error n2 state.word_size (State.dump state);
      Vnum (n1 lsr n2)
    | _ -> assert false)
  | Ealloc typ -> State.alloc state typ
  | Efun _ -> assert false
;;

let eval_exp state e = eval_exp state e |> resolve_value state

let resolve_bound state =
  let open Ast in
  function
  | Oconst i -> Some i
  | Ovar v -> State.get_bound state v
  | Ominus (v, i) -> Option.map (State.get_bound state v) ~f:(fun x -> x - i)
;;

let try_dealloc state x p typ =
  let v = State.get_mem state p in
  let expected = Ir.Lower.default typ in
  if not (Ir.equal_value v expected)
  then Errors.unassign_deallocate_error x (Ealloc typ) expected v (State.dump state);
  State.del_reg state x;
  State.dealloc state p
;;

let check_regs_cleared (state : State.t) (func : Ir.func) =
  let module IdSet = Int.Set in
  let rec returned v =
    match v with
    | Ir.Vvar i -> IdSet.singleton i
    | Vprod vs -> vs |> List.map ~f:returned |> List.fold ~init:IdSet.empty ~f:IdSet.union
    | _ -> IdSet.empty
  in
  let remaining = state.reg |> IdTable.keys |> IdSet.of_list in
  let except_returned =
    IdSet.remove
      (IdSet.diff
         (IdSet.diff remaining (State.get_reg state func.result.name |> returned))
         (List.map func.args ~f:(fun arg -> arg.name) |> IdSet.of_list))
      func.result.name
  in
  assert (IdSet.is_empty except_returned)
;;

let rec invoke (state : State.t) x args (func : Ir.func) forward =
  let reg = IdTable.create () in
  let state' = { state with reg; frame = (forward, func.name) :: state.frame } in
  if !Args.verbose
  then
    Format.printf
      "Invoke %s %s\n%!"
      (Symbol.name func.name)
      (if forward then "forward" else "backward");
  List.iter2_exn args func.args ~f:(fun v arg ->
      State.set_reg state' arg.name (resolve_value state v));
  if forward
  then (
    exec_stmt state' func.body;
    check_regs_cleared state' func;
    State.set_reg state x (State.get_reg state' func.result.name |> resolve_value state'))
  else (
    State.set_reg state' func.result.name (State.get_reg state x |> resolve_value state');
    State.del_reg state x;
    unexec_stmt state' func.body);
  List.iter2_exn args func.args ~f:(fun v arg ->
      let v' = State.get_reg state' arg.name |> resolve_value state' in
      match v with
      | Vvar v -> State.set_reg state v v'
      | v ->
        if not (Ir.equal_value v v')
        then Errors.call_modified_error arg.name func.name v v' (State.dump state))

and call_func (state : State.t) x (f : Ir.call) forward =
  let func = IdTable.find_exn state.funcs f.name in
  match f.bound with
  | None -> invoke state x f.args func forward
  | Some v ->
    (match resolve_bound state v with
    | None -> assert false
    | Some 0 ->
      State.set_reg
        state
        x
        (Ir.Lower.default (Ast.Check.normalize_type state.ctx func.result.typ))
    | Some n ->
      let bound_var =
        match func.bound with
        | Some (Ovar v) -> v
        | _ -> assert false
      in
      let old_bound = State.get_bound state bound_var in
      IdTable.remove state.bounds bound_var;
      State.set_bound state bound_var n;
      invoke state x f.args func forward;
      IdTable.remove state.bounds bound_var;
      Option.iter old_bound ~f:(State.set_bound state bound_var))

and exec_stmt (state : State.t) s =
  match s with
  | Sseq ss -> List.iter ss ~f:(exec_stmt state)
  | Sassign (x, Efun f) -> call_func state x f true
  | Sunassign (x, Efun f) -> call_func state x f false
  | Sassign (x, e) -> State.set_reg state x (eval_exp state e)
  | Sunassign (x, Ealloc typ) ->
    (match State.get_reg state x with
    | Vptr (_, p) -> try_dealloc state x p (Ast.Check.normalize_type state.ctx typ)
    | _ -> assert false)
  | Sunassign (x, e) ->
    let v = State.get_reg state x in
    let v' = eval_exp state e in
    if not (Ir.equal_value v v')
    then Errors.unassign_unequal_error x e v v' (State.dump state);
    State.del_reg state x
  | Sswap (x1, x2) ->
    let v = State.get_reg state x2 in
    State.set_reg state x2 (State.get_reg state x1);
    State.set_reg state x1 v
  | Smem_swap (p, x) ->
    let v = State.get_reg state p in
    (match v with
    | Vptr (_, p) ->
      let v = State.get_mem state p in
      let v' = State.get_reg state x in
      State.set_mem state p v';
      State.set_reg state x v
    | _ -> raise NullDereference)
  | Sif (x, s1, s2) ->
    let v = State.get_reg state x in
    (match v with
    | Vbool true -> exec_stmt state s1
    | Vbool false -> exec_stmt state s2
    | _ -> assert false)
  | Sdebug -> State.debug state

and unexec_stmt (state : State.t) s = exec_stmt state (Ir.Lower.reverse s)

let interp ~mem_size ctx decls =
  let state = State.init ~mem_size ctx in
  let ret = ref None in
  List.iter decls ~f:(function
      | Ir.Dfunc f ->
        IdTable.add_exn state.funcs ~key:f.name ~data:f;
        if String.equal (Symbol.name f.name) "main"
        then (
          exec_stmt state f.body;
          ret := Some (State.get_reg state f.result.name))
      | Dtype _ -> ()
      | Dstatic a ->
        State.alloc_static state (Ast.Check.normalize_type state.ctx a.elt_type) a.values
        |> State.set_reg state a.name);
  if not (State.check_leaks state)
  then
    Format.eprintf
      "Warning: memory may not have been reset to initial state at program termination.\n"
  else Format.eprintf "Exited normally.\n";
  state, !ret
;;
