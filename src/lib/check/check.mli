module IdMap = Symbol.Map

module Context : sig
  type t

  val empty : t
end

(* Obtain a version of a type with aliases substituted at top level *)
val normalize_type : Context.t -> Ast.typ -> Ast.typ

(* Test whether two types are equivalent *)
val equiv_type : Ast.typ -> Ast.typ -> bool

(* Test whether a statement modifies a variable *)
val modifies : Ast.id -> Ast.stmt -> bool

(* Test whether the second bound is smaller than the first *)
val bound_shrunk : Ast.bound option -> Ast.bound option -> bool

(* Get the type of a value *)
val synth_value : Context.t -> Ast.value -> Ast.typ

(* Get the type of an expression *)
val synth_exp : Context.t -> Ast.exp -> Ast.typ

(* Check that a statement is well-formed *)
val check_stmt : Context.t -> Ast.stmt -> Context.t

(* Check that a function body makes bounded recursive calls correctly *)
val check_bound : Ast.func -> Ast.stmt -> unit

(* Check that a function is well-formed *)
val check_func : Context.t -> Ast.func -> unit

(* Check that a type definition is valid and resolve all of its references *)
val check_type_decl : ?under_ptr:bool -> Ast.typ IdMap.t -> Ast.id -> Ast.typ -> Ast.typ

(* Check that a program is well-formed *)
val check_decls : ?init:Context.t -> Ast.decl list -> Context.t
