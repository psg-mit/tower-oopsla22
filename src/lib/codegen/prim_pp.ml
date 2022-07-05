open Core
module Prim_circuit = Prim_circuit_

let show_wires ws = List.map ~f:Int.to_string ws |> String.concat ~sep:", "

let show_gate = function
  | Prim_circuit.Pnot (w, []) -> "not " ^ Int.to_string w
  | Pnot (w, ws) -> Format.sprintf "ctrl %@ not %s" (show_wires (ws @ [ w ]))
;;

let show_modul (m : Prim_circuit.modul) =
  Format.sprintf
    "module '%s'\nheap pointer: [%s]\nargs: [%s]\nmem: [%s], [%s]\noutput: [%s]\n%s"
    (Symbol.name m.name)
    (show_wires m.hp)
    (show_wires m.args)
    (show_wires @@ fst m.mem)
    (show_wires @@ snd m.mem)
    (show_wires m.out_args)
    (Prim_circuit.Gates.to_list m.body
    |> List.map ~f:show_gate
    |> List.map ~f:(fun s -> "  " ^ s)
    |> String.concat ~sep:"\n")
;;

let max_qubit = function
  | Prim_circuit.Pnot (w, ws) ->
    List.max_elt ~compare:Int.compare (w :: ws) |> Option.value_exn
;;

let show_stats (m : Prim_circuit.modul) =
  let l = Prim_circuit.Gates.to_seq m.body in
  Format.sprintf
    "module '%s' stats\ntotal qubits: %d\ntotal gates: %d"
    (Symbol.name m.name)
    (Sequence.map l ~f:max_qubit
    |> Sequence.max_elt ~compare:Int.compare
    |> Option.value ~default:0)
    (Prim_circuit.Gates.length m.body)
;;
