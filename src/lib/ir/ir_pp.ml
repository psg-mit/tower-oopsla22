open Core
open Ir_

let rec show_value : value -> string = function
  | Vvar v -> Symbol.name v
  | Vunit -> "()"
  | Vnum i -> Int.to_string i
  | Vbool b -> Bool.to_string b
  | Vnull -> "null"
  | Vptr (t, p) -> "ptr<" ^ show_typ t ^ ">(" ^ Int.to_string p ^ ")"
  | Vprod vs ->
    List.map ~f:show_value vs |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
;;

let show_exp : exp -> string = function
  | Eval v -> show_value v
  | Eproj (x, i) -> Symbol.name x ^ "." ^ Int.to_string (i + 1)
  | Euop (uop, x) -> show_uop uop ^ " (" ^ Symbol.name x ^ ")"
  | Ebop (bop, x1, x2) ->
    "(" ^ Symbol.name x1 ^ ") " ^ show_bop bop ^ " (" ^ Symbol.name x2 ^ ")"
  | Ealloc t -> "alloc<" ^ show_typ t ^ ">"
  | Efun { name; bound; args } ->
    Symbol.name name
    ^ (match bound with
      | Some b -> show_bound b
      | _ -> "")
    ^ "("
    ^ String.concat ~sep:", " (List.map ~f:show_value args)
    ^ ")"
;;
