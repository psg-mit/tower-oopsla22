val default : Ast.typ -> Ir.value
val reverse : Ir.stmt -> Ir.stmt
val flatten : Ir.stmt -> Ir.stmt
val lower_value : Check.Context.t -> Ast.value -> Ir.value
val lower_exp : Check.Context.t -> Ir.id -> Ast.exp -> Ir.stmt
val lower_stmt : Check.Context.t -> Ast.stmt -> Ir.stmt
val lower_func : Check.Context.t -> Ast.func -> Ir.func
val lower_static : Check.Context.t -> Ast.static -> Ir.static
val lower_decl : Check.Context.t -> Ast.decl -> Ir.decl
