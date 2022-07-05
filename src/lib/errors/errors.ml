module Ast = Ast_
module Ir = Ir_

type static_error =
  | UnknownTypeIdentifier of Ast.id
  | UnknownIdentifier of Ast.id
  | UnknownFunctionIdentifier of Ast.id
  | IdentifierAlreadyBound of Ast.id
  | LocalVariableNotUnbound of Ast.id
  | ArgumentUnbound of Ast.id
  | ArgumentListLength of Ast.id * int * int
  | ArgumentTypeMismatch of Ast.id * Ast.id * Ast.typ * Ast.typ
  | MismatchedBranches of Ast.id
  | ModifiedCondition of Ast.id
  | IncorrectReturnType of Ast.id * Ast.typ * Ast.typ
  | InfiniteType of Ast.id
  | UnexpectedBound of Ast.id
  | MissingBound of Ast.id
  | IncorrectBound of Ast.id * Ast.bound option
  | MalformedBound of Ast.id * Ast.bound
  | StaticInitType of Ast.id * Ast.typ
  | StaticInitLength of Ast.id * int
  | ProjectFrom of Ast.typ * int
  | UnaryOn of Ast.typ
  | BinaryOn of Ast.typ * Ast.typ
  | UnassignType of Ast.id * Ast.typ * Ast.typ
  | SwapType of Ast.id * Ast.typ * Ast.id * Ast.typ
  | MemSwapNotPtr of Ast.exp * Ast.typ
  | MemSwapType of Ast.exp * Ast.typ * Ast.id * Ast.typ
  | IfType of Ast.id * Ast.typ
  | PatternMismatch of Ast.pat * Ast.typ
  | FuncOnTuple of Ast.id * Ast.value
  | DupArg of Ast.id * Ast.id

exception StaticError of static_error

type state_dumper = unit -> unit

exception RuntimeError of string * state_dumper

let verbose = ref false

let call_modified_error x f v1 v2 dump =
  raise
    (RuntimeError
       ( Format.sprintf
           "Call to function '%s' with value %s for argument '%s' should not have \
            modified that argument, got value '%s'"
           (Symbol.name f)
           (Ir_pp.show_value v1)
           (Symbol.name x)
           (Ir_pp.show_value v2)
       , dump ))
;;

let unassign_unequal_error x e v1 v2 dump =
  raise
    (RuntimeError
       ( Format.sprintf
           "Could not uncompute variable '%s' using expression '%s'; expected value \
            '%s', got value '%s'"
           (Symbol.name x)
           (Ir_pp.show_exp e)
           (Ir_pp.show_value v1)
           (Ir_pp.show_value v2)
       , dump ))
;;

let unassign_deallocate_error x e v1 v2 dump =
  raise
    (RuntimeError
       ( Format.sprintf
           "Could not deallocate variable '%s' using expression '%s'; expected value \
            '%s', heap contains '%s'"
           (Symbol.name x)
           (Ir_pp.show_exp e)
           (Ir_pp.show_value v1)
           (Ir_pp.show_value v2)
       , dump ))
;;

let word_size_error n word_size dump =
  raise
    (RuntimeError
       (Format.sprintf "Could not access bit %d beyond word size of %d" n word_size, dump))
;;

let static_init_type_error x t = raise (StaticError (StaticInitType (x, t)))
let static_init_length_error x n = raise (StaticError (StaticInitLength (x, n)))

let static_error_name = function
  | UnknownTypeIdentifier t ->
    Format.sprintf "Unknown type identifier: '%s'" (Symbol.name t)
  | UnknownIdentifier t -> Format.sprintf "Unknown identifier: '%s'" (Symbol.name t)
  | UnknownFunctionIdentifier t ->
    Format.sprintf "Unknown function identifier: '%s'" (Symbol.name t)
  | IdentifierAlreadyBound t ->
    Format.sprintf "Cannot bind already bound identifier: '%s'" (Symbol.name t)
  | LocalVariableNotUnbound t ->
    Format.sprintf "Local variable was not unbound: '%s'" (Symbol.name t)
  | ArgumentUnbound t -> Format.sprintf "Cannot unbound argument: '%s'" (Symbol.name t)
  | ArgumentListLength (a, n, n') ->
    Format.sprintf
      "Cannot call function '%s' with %d arguments, expected %d"
      (Symbol.name a)
      n'
      n
  | ArgumentTypeMismatch (f, a, t, t') ->
    Format.sprintf
      "Argument type mismatch in function '%s' for argument '%s': expected '%s', got '%s'"
      (Symbol.name f)
      (Symbol.name a)
      (Ast_pp.show_typ t)
      (Ast_pp.show_typ t')
  | MismatchedBranches t ->
    Format.sprintf
      "Branches of if-statement mismatched: only one side binds variable '%s'"
      (Symbol.name t)
  | ModifiedCondition t ->
    Format.sprintf "Cannot modify condition '%s' within if-expression" (Symbol.name t)
  | IncorrectReturnType (f, t, t') ->
    Format.sprintf
      "Incorrect return type for function '%s': expected '%s', got '%s'"
      (Symbol.name f)
      (Ast_pp.show_typ t)
      (Ast_pp.show_typ t')
  | InfiniteType t ->
    Format.sprintf
      "Cannot declare recursive type '%s' without pointer indirection; size would be \
       infinite"
      (Symbol.name t)
  | UnexpectedBound f ->
    Format.sprintf "Unexpected bound on function call '%s'" (Symbol.name f)
  | MissingBound f -> Format.sprintf "Missing bound on function call '%s'" (Symbol.name f)
  | IncorrectBound (f, b) ->
    Format.sprintf
      "Cannot call function '%s' without decreased bound: got %s"
      (Symbol.name f)
      (match b with
      | Some b -> "'" ^ Ast_pp.show_bound b ^ "'"
      | None -> "no bound")
  | MalformedBound (f, b) ->
    Format.sprintf
      "Definition of function '%s' must use variable as bound, got '%s'"
      (Symbol.name f)
      (Ast_pp.show_bound b)
  | StaticInitType (x, t) ->
    Format.sprintf
      "Could not initialize static variable '%s' using values not of type '%s'"
      (Symbol.name x)
      (Ast_pp.show_typ t)
  | StaticInitLength (x, n) ->
    Format.sprintf
      "Could not initialize static variable '%s' of using list not of length %d"
      (Symbol.name x)
      n
  | ProjectFrom (t, n) ->
    Format.sprintf
      "Cannot project field %d from incorrect type, got '%s'"
      n
      (Ast_pp.show_typ t)
  | UnaryOn t ->
    Format.sprintf "Cannot apply unary operator to type '%s'" (Ast_pp.show_typ t)
  | BinaryOn (t1, t2) ->
    Format.sprintf
      "Cannot apply binary operator to types '%s', '%s'"
      (Ast_pp.show_typ t1)
      (Ast_pp.show_typ t2)
  | UnassignType (x, t, t') ->
    Format.sprintf
      "Cannot unassign variable '%s' of type '%s' using expression of different type '%s'"
      (Symbol.name x)
      (Ast_pp.show_typ t)
      (Ast_pp.show_typ t')
  | SwapType (x, t, x', t') ->
    Format.sprintf
      "Cannot swap variables '%s' and '%s' of different types '%s' and '%s"
      (Symbol.name x)
      (Symbol.name x')
      (Ast_pp.show_typ t)
      (Ast_pp.show_typ t')
  | MemSwapType (e, t, x', t') ->
    Format.sprintf
      "Cannot swap with memory using '%s' pointing to type '%s' and variable '%s' of \
       different type '%s'"
      (Ast_pp.show_exp e)
      (Ast_pp.show_typ t)
      (Symbol.name x')
      (Ast_pp.show_typ t')
  | MemSwapNotPtr (e, t) ->
    Format.sprintf
      "Cannot swap with memory using '%s' of non-pointer type '%s'"
      (Ast_pp.show_exp e)
      (Ast_pp.show_typ t)
  | IfType (x, t) ->
    Format.sprintf
      "Condition of if-statement cannot be '%s' of non-Boolean type '%s'"
      (Symbol.name x)
      (Ast_pp.show_typ t)
  | PatternMismatch (pat, t) ->
    Format.sprintf
      "Cannot bind type '%s' to pattern '%s'"
      (Ast_pp.show_typ t)
      (Ast_pp.show_pat pat)
  | FuncOnTuple (f, v) ->
    Format.sprintf
      "Cannot call function '%s' on tuple '%s', must bind to variable first"
      (Symbol.name f)
      (Ast_pp.show_value v)
  | DupArg (f, v) ->
    Format.sprintf
      "Cannot call function '%s' with argument '%s' twice, must bind to different \
       variable first"
      (Symbol.name f)
      (Symbol.name v)
;;

let unknown_type_identifier t = raise (StaticError (UnknownTypeIdentifier t))
let unknown_identifier t = raise (StaticError (UnknownIdentifier t))
let unknown_func_identifier t = raise (StaticError (UnknownFunctionIdentifier t))
let identifier_already_bound t = raise (StaticError (IdentifierAlreadyBound t))
let local_not_unbound t = raise (StaticError (LocalVariableNotUnbound t))
let argument_unbound t = raise (StaticError (ArgumentUnbound t))
let argument_list_length a n n' = raise (StaticError (ArgumentListLength (a, n, n')))

let argument_type_mismatch f a t t' =
  raise (StaticError (ArgumentTypeMismatch (f, a, t, t')))
;;

let mismatched_branches t = raise (StaticError (MismatchedBranches t))
let modified_condition t = raise (StaticError (ModifiedCondition t))
let incorrect_return_type f t t' = raise (StaticError (IncorrectReturnType (f, t, t')))
let infinite_type t = raise (StaticError (InfiniteType t))
let unexpected_bound f = raise (StaticError (UnexpectedBound f))
let missing_bound f = raise (StaticError (MissingBound f))
let incorrect_bound f b = raise (StaticError (IncorrectBound (f, b)))
let malformed_bound f b = raise (StaticError (MalformedBound (f, b)))
let project_from t n = raise (StaticError (ProjectFrom (t, n)))
let unary_on t = raise (StaticError (UnaryOn t))
let binary_on t1 t2 = raise (StaticError (BinaryOn (t1, t2)))
let unassign_type x t t' = raise (StaticError (UnassignType (x, t, t')))
let swap_type x t x' t' = raise (StaticError (SwapType (x, t, x', t')))
let mem_swap_type e t x' t' = raise (StaticError (MemSwapType (e, t, x', t')))
let mem_swap_not_ptr e t = raise (StaticError (MemSwapNotPtr (e, t)))
let if_type x t = raise (StaticError (IfType (x, t)))
let pattern_mismatch pat t = raise (StaticError (PatternMismatch (pat, t)))
let func_on_tuple f v = raise (StaticError (FuncOnTuple (f, v)))
let dup_arg f v = raise (StaticError (DupArg (f, v)))
