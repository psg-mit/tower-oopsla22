module Lir := Lir_

module Context : sig
  type t

  val empty : Ir.func list -> t
end

val type_size : Ir.typ -> int
val default : Lir.typ -> Lir.value
val lower_value : Ir.value -> Lir.value
val lower_exp : Context.t -> Lir.id -> Ir.exp -> Lir.stmt list
val lower_stmt : Context.t -> Ir.stmt -> Context.t * Lir.stmt list
val lower_func : Ir.func list -> Ir.func -> Lir.modul
