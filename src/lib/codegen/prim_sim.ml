open Core
module State = Int.Table
open Prim_circuit_

type state = bool State.t

let get ctx w = State.find_or_add ctx w ~default:(fun () -> false)

let sim_gate ctx = function
  | Pnot (w, cs) ->
    if List.for_all ~f:(get ctx) cs
    then (
      let v = get ctx w in
      State.set ctx ~key:w ~data:(not v))
    else ()
;;

let sim_gates ctx = Gates.iter ~f:(sim_gate ctx)

let check_regs_cleared state m =
  let remaining =
    state |> State.filter ~f:(fun x -> x) |> State.keys |> Int.Set.of_list
  in
  let out_arg = m.out_args |> Int.Set.of_list in
  let hp = m.hp |> Int.Set.of_list in
  let mem = snd m.mem |> Int.Set.of_list in
  let remaining = Int.Set.diff (Int.Set.diff (Int.Set.diff remaining out_arg) hp) mem in
  assert (Int.Set.is_empty @@ remaining)
;;

let check_leaks state m =
  let ctrl, mem = m.mem in
  let mem_size = List.length ctrl in
  let word_size = List.length mem / mem_size in
  let final_mem =
    List.map mem ~f:(fun x ->
        if State.find_or_add state ~default:(fun () -> false) x then "1" else "0")
    |> List.chunks_of ~length:word_size
    |> List.map ~f:(fun l ->
           String.concat l ~sep:"" |> String.rev |> fun s -> "0b" ^ s |> Int.of_string)
  in
  let initial_mem =
    List.init mem_size ~f:(fun i -> if i = mem_size - 1 then 0 else i + 1)
  in
  let initial_mem = Int.Set.add (Int.Set.of_list initial_mem) 1 in
  let final_mem = Int.Set.union (Int.Set.of_list final_mem) @@ Int.Set.of_list m.hp in
  Int.Set.equal initial_mem final_mem && Int.Set.mem final_mem 0
;;

let interp (modules : modul list) =
  let state = State.create () in
  List.iter modules ~f:(fun m ->
      if m.name = Symbol.get_sym "main"
      then (
        assert (List.is_empty m.args);
        sim_gates state m.body;
        check_regs_cleared state m;
        if not (check_leaks state m)
        then
          Format.eprintf
            "Warning: memory may not have been reset to initial state at program \
             termination.\n"
        else Format.eprintf "Exited normally.\n"));
  state
;;

let show_reg state =
  State.to_alist state
  |> List.sort ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2)
  |> List.map ~f:(fun (i, v) -> Format.sprintf "%d -> %s" i (Bool.to_string v))
  |> String.concat ?sep:(Some ", ")
  |> fun s -> "{ " ^ s ^ " }"
;;
