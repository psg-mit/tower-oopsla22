open Core
open Ir_

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

let show_exp : exp -> string = function
  | Eval v -> show_value v
  | Eproj (x, i) -> Symbol.name x ^ "." ^ Int.to_string i
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

let indent = List.map ~f:(fun s -> "  " ^ s)

let rec show_stmt : stmt -> string list = function
  | Sseq ss -> List.concat_map ~f:show_stmt ss
  | Sassign (x, e) -> [ "let " ^ Symbol.name x ^ " <- " ^ show_exp e ^ ";" ]
  | Sunassign (x, e) -> [ "let " ^ Symbol.name x ^ " -> " ^ show_exp e ^ ";" ]
  | Sswap (x, y) -> [ Symbol.name x ^ " <-> " ^ Symbol.name y ^ ";" ]
  | Smem_swap (x, y) -> [ "*" ^ Symbol.name x ^ " <-> " ^ Symbol.name y ^ ";" ]
  | Sif (x, s1, s2) ->
    [ "if " ^ Symbol.name x ^ " {" ]
    @ indent (show_stmt s1)
    @ [ "} else {" ]
    @ indent (show_stmt s2)
    @ [ "}" ]
  | Sdebug -> [ "***;" ]
;;

let show_id_typ (i : id_typ) = Symbol.name i.name ^ ": " ^ show_typ i.typ

let show_func (f : func) : string =
  let args = "(" ^ String.concat ~sep:", " (List.map ~f:show_id_typ f.args) ^ ")" in
  let ret = " -> " ^ show_typ f.result.typ in
  let bound =
    match f.bound with
    | None -> ""
    | Some b -> show_bound b
  in
  let body = String.concat ~sep:"\n" (indent (show_stmt f.body)) in
  "fun " ^ Symbol.name f.name ^ bound ^ args ^ ret ^ " {\n" ^ body ^ "\n}"
;;

let show_decl : decl -> string = function
  | Dstatic s -> show_static s
  | Dtype i -> "type " ^ Symbol.name i.name ^ " = " ^ show_typ i.typ ^ ";"
  | Dfunc f -> show_func f
;;

let show_decls (decls : decl list) : string =
  List.map ~f:show_decl decls |> String.concat ~sep:"\n"
;;
