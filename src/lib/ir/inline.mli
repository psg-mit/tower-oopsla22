module Ir := Ir_
module IdMap := Symbol.Map

val rename_id : Ir.id IdMap.t -> Ir.id -> Ir.id
val rename_val : Ir.id IdMap.t -> Ir.value -> Ir.value
val rename_exp : Ir.id IdMap.t -> Ir.exp -> Ir.exp
val rename_stmt : Ir.id IdMap.t -> Ir.stmt -> Ir.stmt
val inline : ?bounds:int IdMap.t -> Ir.decl list -> Ir.func list
