open Core
open Lir_

let rec show_typ : typ -> string = function
  | Tunit -> "()"
  | Tbool -> "bool"
  | Tuint -> "uint"
  | Tprod ts ->
    List.map ~f:show_typ ts |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
;;

let rec show_value : value -> string = function
  | Vvar v -> Symbol.name v
  | Vunit -> "()"
  | Vnum i -> Int.to_string i
  | Vbool b -> Bool.to_string b
  | Vprod vs ->
    List.map ~f:show_value vs |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
;;

let show_uop : uop -> string = function
  | Ulnot -> "lnot"
;;

let show_bop : bop -> string = function
  | Bplus -> "+"
  | Bminus -> "-"
  | Btimes -> "*"
  | Beq -> "=="
  | Bless -> "<"
  | Bland -> "land"
  | Blor -> "lor"
  | Blsl -> "lsl"
  | Blsr -> "lsr"
;;

let show_exp : exp -> string = function
  | Eval v -> show_value v
  | Eproj (x, _, i) -> Symbol.name x ^ "." ^ Int.to_string i
  | Euop (uop, x) -> show_uop uop ^ " (" ^ Symbol.name x ^ ")"
  | Ebop (bop, x1, x2) ->
    "(" ^ Symbol.name x1 ^ ") " ^ show_bop bop ^ " (" ^ Symbol.name x2 ^ ")"
  | Ealloc t -> "alloc<" ^ show_typ t ^ ">"
;;

let indent = List.map ~f:(fun s -> "  " ^ s)

let rec show_stmt : stmt -> string list = function
  | Sassign (t, x, e) ->
    [ "let " ^ Symbol.name x ^ ": " ^ show_typ t ^ " <- " ^ show_exp e ^ ";" ]
  | Sunassign (t, x, e) ->
    [ "let " ^ Symbol.name x ^ ": " ^ show_typ t ^ " -> " ^ show_exp e ^ ";" ]
  | Sswap (x, y) -> [ Symbol.name x ^ " <-> " ^ Symbol.name y ^ ";" ]
  | Smem_swap (x, y) -> [ "*" ^ Symbol.name x ^ " <-> " ^ Symbol.name y ^ ";" ]
  | Sif (x, ss) ->
    [ "if " ^ Symbol.name x ^ " {" ] @ indent (List.concat_map ~f:show_stmt ss) @ [ "}" ]
;;

let show_module (m : modul) =
  let args =
    "("
    ^ String.concat
        ~sep:", "
        (List.map ~f:(fun (t, x) -> Symbol.name x ^ ": " ^ show_typ t) m.args)
    ^ ")"
  in
  let t, out_arg = m.out_arg in
  let ret = " -> " ^ Symbol.name out_arg ^ ": " ^ show_typ t in
  let body = String.concat ~sep:"\n" (indent (List.concat_map ~f:show_stmt m.body)) in
  "fun " ^ Symbol.name m.name ^ args ^ ret ^ " {\n" ^ body ^ "\n}"
;;

let show_modules (ms : modul list) : string =
  List.map ~f:show_module ms |> String.concat ~sep:"\n"
;;

let rec length = function
  | Sif (_, s) -> 1 + List.sum (module Int) ~f:length s
  | Sswap _ | Sassign _ | Sunassign _ | Smem_swap _ -> 1
;;

let show_stats (m : modul) =
  Format.sprintf
    "module '%s' stats\nstatements: %d"
    (Symbol.name m.name)
    (List.sum (module Int) ~f:length m.body)
;;
