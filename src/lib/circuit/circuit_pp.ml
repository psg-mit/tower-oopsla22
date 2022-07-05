open Core
module Circuit = Circuit_

let show_wires ws = "[" ^ (List.map ~f:Int.to_string ws |> String.concat ~sep:", ") ^ "]"
let indent = List.map ~f:(fun s -> "  " ^ s)

let rec show_gate = function
  | Circuit.Gnum (n, ws) -> [ "set " ^ Int.to_string n ^ " -> " ^ show_wires ws ]
  | Gbool (n, w) -> [ "set " ^ Bool.to_string n ^ " -> " ^ Int.to_string w ]
  | Gnot ws -> [ "not " ^ show_wires ws ]
  | Gcopy (ws1, ws2) -> [ "copy " ^ show_wires ws1 ^ " -> " ^ show_wires ws2 ]
  | Gbop (bop, ws1, ws2, ws3) ->
    let bop =
      match bop with
      | Bland -> "land"
      | Blor -> "lor"
      | Beq -> "eq"
      | Blsl -> "lsl"
      | Blsr -> "lsr"
      | Bplus -> "add"
      | Bminus -> "sub"
      | Btimes -> "mul"
      | Bless -> "le"
    in
    [ bop ^ " " ^ show_wires ws1 ^ "; " ^ show_wires ws2 ^ " -> " ^ show_wires ws3 ]
  | Gswap (ws1, ws2) -> [ "swap " ^ show_wires ws1 ^ " <-> " ^ show_wires ws2 ]
  | Gmem_swap (ws1, ws2) -> [ "mem " ^ show_wires ws1 ^ " <-> " ^ show_wires ws2 ]
  | Gcond (w, gs) ->
    ("cond " ^ Int.to_string w) :: indent (List.concat_map ~f:show_gate gs)
  | Gconj gs -> "conj" :: indent (List.concat_map ~f:show_gate gs)
  | Ginit w -> [ "init " ^ Int.to_string w ]
;;

let show_modul (m : Circuit.modul) =
  Format.sprintf
    "module %s:\nheap pointer: [%s]\nargs: [%s]\noutput: [%s]\n%s"
    (Symbol.name m.name)
    (List.map ~f:Int.to_string m.hp |> String.concat ~sep:", ")
    (List.map ~f:Int.to_string m.out_args |> String.concat ~sep:", ")
    (List.map ~f:Int.to_string m.args |> String.concat ~sep:", ")
    (String.concat ~sep:"\n" (indent (List.concat_map ~f:show_gate m.body)))
;;

let rec all_qubits = function
  | Circuit.Ginit w -> Int.Set.singleton w
  | Gcond (_, g) | Gconj g ->
    Sequence.of_list g
    |> Sequence.map ~f:all_qubits
    |> Sequence.fold ~init:Int.Set.empty ~f:Int.Set.union
  | Gnum _ | Gbool _ | Gnot _ | Gcopy _ | Gbop _ | Gswap _ | Gmem_swap _ -> Int.Set.empty
;;

let all_qubits gs =
  Sequence.of_list gs
  |> Sequence.map ~f:all_qubits
  |> Sequence.fold ~init:Int.Set.empty ~f:Int.Set.union
;;

let rec length = function
  | Circuit.Gcond (_, g) | Gconj g -> List.sum (module Int) ~f:length g
  | Ginit _ -> 0
  | Gcopy _ | Gswap _ -> 0
  | Gnum _ | Gbool _ | Gnot _ | Gbop _ | Gmem_swap _ -> 1
;;

let show_stats (m : Circuit.modul) =
  Format.sprintf
    "module '%s' stats\ntotal qubits: %d\nlogic/arith/mem gates: %d"
    (Symbol.name m.name)
    (Int.Set.length @@ all_qubits m.body)
    (Int.max 1 @@ List.sum (module Int) ~f:length m.body)
;;
