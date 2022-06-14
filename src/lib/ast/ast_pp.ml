open Core
open Ast_

let rec show_typ : typ -> string = function
  | Tvar i -> Symbol.name i
  | Tunit -> "()"
  | Tbool -> "bool"
  | Tuint -> "uint"
  | Tprod ts ->
    List.map ~f:show_typ ts |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
  | Tptr None -> "ptr<unknown>"
  | Tptr (Some t) -> "ptr<" ^ show_typ t ^ ">"
;;

let rec show_value : value -> string = function
  | Vvar v -> Symbol.name v
  | Vunit -> "()"
  | Vnum i -> Int.to_string i
  | Vbool b -> Bool.to_string b
  | Vnull -> "null"
  | Vptr (t, p) -> "ptr<" ^ show_typ t ^ ">(" ^ Int.to_string p ^ ")"
  | Vdefault t -> "default<" ^ show_typ t ^ ">"
  | Vprod vs ->
    List.map ~f:show_value vs |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
;;

let show_bound : bound -> string = function
  | Ovar b -> "[" ^ Symbol.name b ^ "]"
  | Ominus (b, i) -> "[" ^ Symbol.name b ^ "-" ^ Int.to_string i ^ "]"
  | Oconst i -> "[" ^ Int.to_string i ^ "]"
;;

let show_uop : uop -> string = function
  | Unot -> "not"
  | Utest -> "test"
  | Ulnot -> "lnot"
;;

let show_bop : bop -> string = function
  | Band -> "&&"
  | Bor -> "||"
  | Bplus -> "+"
  | Bminus -> "-"
  | Btimes -> "*"
  | Beq -> "=="
  | Bneq -> "!="
  | Bless -> "<"
  | Bgreater -> ">"
  | Ble -> "<="
  | Bge -> ">="
  | Bland -> "land"
  | Blor -> "lor"
  | Blsl -> "lsl"
  | Blsr -> "lsr"
;;

let rec show_pat : pat -> string = function
  | Pid x -> Symbol.name x
  | Punit -> "()"
  | Puint i -> Int.to_string i
  | Pbool b -> Bool.to_string b
  | Pnull -> "null"
  | Pprod ps ->
    List.map ~f:show_pat ps |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
;;

let rec show_exp : exp -> string = function
  | Eval v -> show_value v
  | Eproj (e, i) -> show_exp e ^ "." ^ Int.to_string (i + 1)
  | Euop (uop, e) -> show_uop uop ^ " (" ^ show_exp e ^ ")"
  | Ebop (bop, e1, e2) ->
    "(" ^ show_exp e1 ^ ") " ^ show_bop bop ^ " (" ^ show_exp e2 ^ ")"
  | Ealloc t -> "alloc<" ^ show_typ t ^ ">"
  | Efun { name; bound; args } ->
    Symbol.name name
    ^ (match bound with
      | Some b -> show_bound b
      | _ -> "")
    ^ "("
    ^ String.concat ~sep:", " (List.map ~f:show_value args)
    ^ ")"
  | Eprod es ->
    List.map ~f:show_exp es |> String.concat ~sep:", " |> fun s -> "(" ^ s ^ ")"
;;
