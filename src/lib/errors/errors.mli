module Ast := Ast_
module Ir := Ir_

val verbose : bool ref

(* Static errors *)
type static_error

val static_error_name : static_error -> string

exception StaticError of static_error

val static_init_type_error : Ast.id -> Ast.typ -> 'a
val static_init_length_error : Ast.id -> int -> 'a
val unknown_type_identifier : Ast.id -> 'a
val unknown_identifier : Ast.id -> 'a
val unknown_func_identifier : Ast.id -> 'a
val identifier_already_bound : Ast.id -> 'a
val local_not_unbound : Ast.id -> 'a
val argument_unbound : Ast.id -> 'a
val argument_list_length : Ast.id -> int -> int -> 'a
val argument_type_mismatch : Ast.id -> Ast.id -> Ast.typ -> Ast.typ -> 'a
val mismatched_branches : Ast.id -> 'a
val modified_condition : Ast.id -> 'a
val incorrect_return_type : Ast.id -> Ast.typ -> Ast.typ -> 'a
val infinite_type : Ast.id -> 'a
val unexpected_bound : Ast.id -> 'a
val missing_bound : Ast.id -> 'a
val incorrect_bound : Ast.id -> Ast.bound option -> 'a
val malformed_bound : Ast.id -> Ast.bound -> 'a
val project_from : Ast.typ -> int -> 'a
val unary_on : Ast.typ -> 'a
val binary_on : Ast.typ -> Ast.typ -> 'a
val unassign_type : Ast.id -> Ast.typ -> Ast.typ -> 'a
val swap_type : Ast.id -> Ast.typ -> Ast.id -> Ast.typ -> 'a
val mem_swap_type : Ast.exp -> Ast.typ -> Ast.id -> Ast.typ -> 'a
val mem_swap_not_ptr : Ast.exp -> Ast.typ -> 'a
val if_type : Ast.id -> Ast.typ -> 'a
val pattern_mismatch : Ast.pat -> Ast.typ -> 'a
val func_on_tuple : Ast.id -> Ast.value -> 'a
val dup_arg : Ast.id -> Ast.id -> 'a

(* Runtime errors *)
type state_dumper = unit -> unit

exception RuntimeError of string * state_dumper

val call_modified_error : Ast.id -> Ast.id -> Ir.value -> Ir.value -> state_dumper -> 'a

val unassign_unequal_error
  :  Ast.id
  -> Ir.exp
  -> Ir.value
  -> Ir.value
  -> state_dumper
  -> 'a

val unassign_deallocate_error
  :  Ast.id
  -> Ir.exp
  -> Ir.value
  -> Ir.value
  -> state_dumper
  -> 'a

val word_size_error : int -> int -> state_dumper -> 'a
