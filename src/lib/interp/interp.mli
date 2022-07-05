module IdTable := Symbol.Table

exception OutOfMemory

module State : sig
  type t

  val init : mem_size:int -> Ast.Check.Context.t -> t
  val show_reg : t -> string
  val show_mem : t -> string
  val check_leaks : t -> bool
  val show_static : t -> string
  val print_backtrace : t -> unit
  val debug : t -> unit
  val dump : t -> unit -> unit
end

val resolve_value : State.t -> Ir.value -> Ir.value
val eval_exp : State.t -> Ir.exp -> Ir.value
val resolve_bound : State.t -> Ast.bound -> int option
val exec_stmt : State.t -> Ir.stmt -> unit
val unexec_stmt : State.t -> Ir.stmt -> unit
val interp : mem_size:int -> Ast.Check.Context.t -> Ir.decl list -> State.t * Ir.value option
