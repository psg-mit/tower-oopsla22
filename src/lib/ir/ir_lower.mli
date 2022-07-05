module Ir := Ir_

val default : Ast.typ -> Ir.value
val reverse : Ir.stmt -> Ir.stmt
val flatten : Ir.stmt -> Ir.stmt
val lower_value : Ast.Check.Context.t -> Ast.value -> Ir.value
val lower_exp : Ast.Check.Context.t -> Ir.id -> Ast.exp -> Ir.stmt
val lower_stmt : Ast.Check.Context.t -> Ast.stmt -> Ir.stmt
val lower_func : Ast.Check.Context.t -> Ast.func -> Ir.func
val lower_static : Ast.Check.Context.t -> Ast.static -> Ir.static
val lower_decl : Ast.Check.Context.t -> Ast.decl -> Ir.decl
val lower_decls : Ast.Check.Context.t -> Ast.decl list -> Ir.decl list
