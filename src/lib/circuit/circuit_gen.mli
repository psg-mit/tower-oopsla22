open Core
module Circuit := Circuit_

module Context : sig
  type t
end

val lower_value : Context.t -> Circuit.wire list -> Circuit.gate Queue.t -> Lir.value -> unit
val lower_exp : Context.t -> Circuit.wire list -> Circuit.gate Queue.t -> Lir.exp -> unit
val lower_stmt : Context.t -> Alloc.t -> Circuit.gate Queue.t -> Lir.stmt -> Context.t
val lower_module : word_size:int -> Lir.modul -> Circuit.modul
