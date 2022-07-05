module IdMap := Symbol.Map
module Ir := Ir_

module Context : sig
  type t

  val empty : Ir.func list -> t
  val add_local : t -> int -> Ast.typ -> t
  val get_local : t -> int -> Ast.typ
  val remove_local : t -> int -> t
end

val synth_value : Context.t -> Ir.value -> Ast.typ
val synth_exp : Context.t -> Ir.exp -> Ast.typ
