open Core
module Ast = Ast_
open Ast
module IdMap = Symbol.Map
module IdSet = Symbol.Set

module Context = struct
  type func =
    { is_bounded : bool
    ; arg_types : (id * typ) list
    ; result_type : typ
    }

  type t =
    { type_aliases : Ast.typ IdMap.t
    ; funcs : func IdMap.t
    ; statics : typ IdMap.t
    ; locals : typ IdMap.t
    ; bound_var : id option
    }

  let empty : t =
    { type_aliases = IdMap.empty
    ; funcs = IdMap.empty
    ; statics = IdMap.empty
    ; locals = IdMap.empty
    ; bound_var = None
    }
  ;;

  let add_type ctx key data =
    match IdMap.add ctx.type_aliases ~key ~data with
    | `Ok ctx -> Some ctx
    | _ -> None
  ;;

  let get_type ctx key = IdMap.find ctx.type_aliases key
end

let rec normalize_type (ctx : Context.t) = function
  | Tvar t ->
    (match Context.get_type ctx t with
    | Some t' -> normalize_type ctx t'
    | None -> Errors.unknown_type_identifier t)
  | Tunit -> Tunit
  | Tbool -> Tbool
  | Tuint -> Tuint
  | Tprod ts -> Tprod (List.map ~f:(normalize_type ctx) ts)
  | Tptr t -> Tptr t
;;

let rec equiv_type t1 t2 =
  match t1, t2 with
  | Tvar i1, Tvar i2 -> Ast.equal_id i1 i2
  | Tunit, Tunit | Tbool, Tbool | Tuint, Tuint -> true
  | Tprod ts1, Tprod ts2 ->
    (match List.for_all2 ts1 ts2 ~f:equiv_type with
    | Ok v -> v
    | _ -> false)
  | Tptr None, Tptr _ | Tptr _, Tptr None -> true
  | Tptr (Some t1), Tptr (Some t2) when equiv_type t1 t2 -> true
  | _ -> false
;;

let type_norm_equiv ctx t1 t2 = equiv_type (normalize_type ctx t1) (normalize_type ctx t2)

let synth_id (ctx : Context.t) i =
  match IdMap.find ctx.locals i with
  | Some t -> t
  | None ->
    (match IdMap.find ctx.statics i with
    | Some t -> t
    | None -> Errors.unknown_identifier i)
;;

let rec synth_value' (ctx : Context.t) = function
  | Vvar i -> synth_id ctx i
  | Vunit -> Tunit
  | Vnum _ -> Tuint
  | Vbool _ -> Tbool
  | Vnull -> Tptr None
  | Vptr (t, _) -> Tptr (Some t)
  | Vdefault t -> t
  | Vprod vs -> Tprod (List.map ~f:(synth_value ctx) vs)

and synth_value ctx v = synth_value' ctx v |> normalize_type ctx

let check_dups args f =
  (List.fold args ~init:IdSet.empty ~f:(fun acc -> function
     | Vvar v -> if IdSet.mem acc v then Errors.dup_arg f v else IdSet.add acc v
     | _ -> acc)
    : IdSet.t)
  |> ignore
;;

let check_args_match ctx args (arg_types : (id * typ) list) f =
  match
    List.iter2 args arg_types ~f:(fun t (name, t') ->
        if not (type_norm_equiv ctx t t') then Errors.argument_type_mismatch f name t' t)
  with
  | Ok _ -> ()
  | _ -> Errors.argument_list_length f (List.length args) (List.length arg_types)
;;

let rec synth_exp' (ctx : Context.t) = function
  | Eval v -> synth_value ctx v
  | Eprod es -> Tprod (List.map es ~f:(synth_exp ctx))
  | Eproj (v, i) ->
    (match synth_exp ctx v with
    | Tprod ts ->
      (match List.nth ts i with
      | Some t -> t
      | None -> Errors.project_from (Tprod ts) i)
    | t -> Errors.project_from t i)
  | Euop (uop, v) ->
    (match uop, synth_exp ctx v with
    | Unot, Tbool | Utest, Tuint | Utest, Tptr _ -> Tbool
    | Ulnot, Tuint -> Tuint
    | _, t -> Errors.unary_on t)
  | Ebop (bop, v1, v2) ->
    let t1 = synth_exp ctx v1 in
    let t2 = synth_exp ctx v2 in
    (match bop, t1, t2 with
    | (Band | Bor | Beq | Bneq | Bless | Bgreater | Ble | Bge), Tbool, Tbool -> Tbool
    | (Beq | Bneq | Bless | Bgreater | Ble | Bge), Tuint, Tuint -> Tbool
    | (Beq | Bneq), Tptr _, Tptr _ -> Tbool
    | (Bplus | Bminus | Btimes), Tuint, Tuint -> Tuint
    | (Bplus | Bminus), Tptr t, Tuint -> Tptr t
    | (Bland | Blor | Blsl | Blsr), Tuint, Tuint -> Tuint
    | _, t1, t2 -> Errors.binary_on t1 t2)
  | Ealloc t -> Tptr (Some t)
  | Efun { name; bound; args } ->
    (match IdMap.find ctx.funcs name with
    | None -> Errors.unknown_func_identifier name
    | Some { is_bounded; arg_types; result_type } ->
      (match is_bounded, bound, ctx.bound_var with
      | false, None, _ -> ()
      | false, Some _, _ -> Errors.unexpected_bound name
      | true, Some (Ovar b | Ominus (b, _)), Some b' when equal_id b b' -> ()
      | true, Some (Ovar b | Ominus (b, _)), _ -> Errors.unknown_identifier b
      | true, Some (Oconst _), _ -> ()
      | true, None, _ -> Errors.missing_bound name);
      List.iter args ~f:(function
          | Vprod vs -> Errors.func_on_tuple name (Vprod vs)
          | _ -> ());
      check_dups args name;
      let args = List.map ~f:(synth_value ctx) args in
      check_args_match ctx args arg_types name;
      result_type)

and synth_exp ctx e = synth_exp' ctx e |> normalize_type ctx

let rec pat_modifies x = function
  | Pid y -> equal_id x y
  | Pprod ps -> List.exists ps ~f:(pat_modifies x)
  | _ -> false
;;

let rec modifies x = function
  | Sassign (p, _) | Sunassign (p, _) -> pat_modifies x p
  | Smem_swap (_, y) -> equal_id x y
  | Sswap (y1, y2) -> equal_id x y1 || equal_id x y2
  | Sif (_, s1, s2) -> modifies x s1 || modifies x s2
  | Sseq ss -> List.exists ss ~f:(modifies x)
  | Sdebug -> false
  | Swith (s1, s2) -> modifies x s1 || modifies x s2
;;

let rec check_pat forward (ctx : Context.t) pat t =
  match pat, t with
  | Pid key, data ->
    if forward
    then (
      match IdMap.add ctx.locals ~key ~data with
      | `Ok locals -> { ctx with locals }
      | `Duplicate -> Errors.identifier_already_bound key)
    else (
      match IdMap.find ctx.locals key with
      | Some t ->
        if type_norm_equiv ctx t data
        then { ctx with locals = IdMap.remove ctx.locals key }
        else Errors.unassign_type key (normalize_type ctx t) data
      | None -> Errors.unknown_identifier key)
  | Punit, Tunit | Pbool _, Tbool | Puint _, Tuint | Pnull, Tptr _ -> ctx
  | Pprod ps, Tprod ts ->
    (match List.fold2 ps ts ~init:ctx ~f:(check_pat forward) with
    | Ok ctx -> ctx
    | _ -> Errors.pattern_mismatch pat t)
  | _, _ -> Errors.pattern_mismatch pat t
;;

let rec reverse (stmt : stmt) : stmt =
  match stmt with
  | Sseq ss -> Sseq (List.rev (List.map ~f:reverse ss))
  | Sassign (x, e) -> Sunassign (x, e)
  | Sunassign (x, e) -> Sassign (x, e)
  | Sswap (x1, x2) -> Sswap (x1, x2)
  | Smem_swap (x1, x2) -> Smem_swap (x1, x2)
  | Sif (x, s1, s2) -> Sif (x, reverse s1, reverse s2)
  | Sdebug -> Sdebug
  | Swith (s1, s2) -> Swith (s1, reverse s2)
;;

let rec check_stmt (ctx : Context.t) = function
  | Sseq ss -> List.fold ss ~init:ctx ~f:check_stmt
  | Sassign (pat, e) ->
    let t = synth_exp ctx e in
    check_pat true ctx pat t
  | Sunassign (pat, e) ->
    let t = synth_exp ctx e in
    check_pat false ctx pat t
  | Sswap (x1, x2) ->
    (match IdMap.find ctx.locals x1, IdMap.find ctx.locals x2 with
    | Some t1, Some t2 ->
      if type_norm_equiv ctx t1 t2 then ctx else Errors.swap_type x1 t1 x2 t2
    | None, _ -> Errors.unknown_identifier x1
    | _, None -> Errors.unknown_identifier x2)
  | Smem_swap (e1, x2) ->
    (match synth_exp ctx e1, IdMap.find ctx.locals x2 with
    | t1, Some t2 ->
      (match normalize_type ctx t1 with
      | Tptr None -> ctx
      | Tptr (Some t1') ->
        if type_norm_equiv ctx t1' t2 then ctx else Errors.mem_swap_type e1 t1' x2 t2
      | _ -> Errors.mem_swap_not_ptr e1 t1)
    | _, None -> Errors.unknown_identifier x2)
  | Sif (x, s1, s2) ->
    (match Option.map ~f:(normalize_type ctx) (IdMap.find ctx.locals x) with
    | Some Tbool ->
      if modifies x s1 || modifies x s2
      then Errors.modified_condition x
      else (
        let ctx1, ctx2 = check_stmt ctx s1, check_stmt ctx s2 in
        let diff =
          IdMap.symmetric_diff ~data_equal:(type_norm_equiv ctx) ctx1.locals ctx2.locals
        in
        match Sequence.hd diff with
        | None -> ctx1
        | Some (x, _) -> Errors.mismatched_branches x)
    | Some t -> Errors.if_type x t
    | None -> Errors.unknown_identifier x)
  | Sdebug -> ctx
  | Swith (s1, s2) -> check_stmt ctx (Sseq [ s1; s2; reverse s1 ])
;;

let bound_shrunk b b' =
  match b, b' with
  | None, None -> true
  | None, Some _ -> assert false
  | Some _, None -> false
  | Some b, Some b' ->
    (match b, b' with
    | Oconst _, _ | (Ovar _ | Ominus _), (Ovar _ | Oconst _) -> false
    | Ovar b, Ominus (b', i) -> equal_id b b' && i > 0
    | Ominus (b, i), Ominus (b', j) -> equal_id b b' && j > i)
;;

let rec check_bound (f : Ast.func) = function
  | Sassign (_, Efun call) | Sunassign (_, Efun call) ->
    if equal_id f.name call.name && not (bound_shrunk f.bound call.bound)
    then Errors.incorrect_bound f.name call.bound
  | Sseq ss -> List.iter ss ~f:(check_bound f)
  | Sif (_, s1, s2) | Swith (s1, s2) ->
    check_bound f s1;
    check_bound f s2
  | Sassign (_, _) | Sunassign (_, _) | Sswap _ | Smem_swap _ | Sdebug -> ()
;;

let check_func (ctx : Context.t) (f : Ast.func) =
  let locals =
    List.fold f.args ~init:ctx.locals ~f:(fun locals arg ->
        match IdMap.add locals ~key:arg.name ~data:arg.typ with
        | `Ok m -> m
        | _ -> Errors.identifier_already_bound arg.name)
  in
  let bound_var =
    Option.bind f.bound ~f:(function
        | Ovar b | Ominus (b, _) -> Some b
        | Oconst _ -> None)
  in
  let ctx = { ctx with locals; bound_var } in
  let ctx = check_stmt ctx f.body in
  let result_value, result_type = f.result in
  let t = synth_value ctx result_value in
  if not (type_norm_equiv ctx t result_type)
  then Errors.incorrect_return_type f.name t result_type;
  let old_locals = IdMap.key_set locals in
  let new_locals = IdMap.key_set ctx.locals in
  let added = IdSet.diff new_locals old_locals in
  let returned v =
    match v with
    | Vvar i -> IdSet.singleton i
    | _ -> IdSet.empty
  in
  IdSet.diff added (returned result_value)
  |> IdSet.choose
  |> Option.iter ~f:Errors.local_not_unbound;
  let removed = IdSet.diff old_locals new_locals in
  IdSet.choose removed |> Option.iter ~f:Errors.argument_unbound;
  Option.iter f.bound ~f:(function
      | Ovar _ -> check_bound f f.body
      | (Ominus _ | Oconst _) as b -> Errors.malformed_bound f.name b)
;;

let rec check_type_decl ?(under_ptr = false) ctx i = function
  | Tvar v when equal_id v i -> if under_ptr then Tvar v else Errors.infinite_type v
  | Tvar v ->
    (match Context.get_type ctx v with
    | Some t -> t
    | None -> Errors.unknown_type_identifier v)
  | Tunit -> Tunit
  | Tbool -> Tbool
  | Tuint -> Tuint
  | Tprod ts -> Tprod (List.map ts ~f:(check_type_decl ~under_ptr ctx i))
  | Tptr (Some t) ->
    Tptr
      (Some
         (let (_ : typ) = check_type_decl ~under_ptr:true ctx i t in
          t))
  | Tptr None -> Tptr None
;;

let check_decls ?(init = Context.empty) (decls : Ast.decl list) =
  List.fold decls ~init ~f:(fun ctx decl ->
      match decl with
      | Dtype { name = key; typ } ->
        let data = check_type_decl ctx key typ in
        let type_aliases =
          match Context.add_type ctx key data with
          | Some m -> m
          | _ -> Errors.identifier_already_bound key
        in
        { ctx with type_aliases }
      | Dfunc f ->
        let is_bounded = Option.is_some f.bound in
        let arg_types = List.map f.args ~f:(fun arg -> arg.name, arg.typ) in
        let data : Context.func = { is_bounded; arg_types; result_type = snd f.result } in
        let funcs =
          match IdMap.add ctx.funcs ~key:f.name ~data with
          | `Ok m -> m
          | _ -> Errors.identifier_already_bound f.name
        in
        let ctx = { ctx with funcs } in
        check_func ctx f;
        { ctx with locals = IdMap.empty; bound_var = None }
      | Dstatic a ->
        Option.iter a.values ~f:(fun vs ->
            if List.length vs = a.size
            then
              if List.for_all vs ~f:(fun v ->
                     type_norm_equiv ctx (synth_value ctx v) a.elt_type)
              then ()
              else Errors.static_init_type_error a.name a.elt_type
            else Errors.static_init_length_error a.name a.size);
        let statics =
          match IdMap.add ctx.statics ~key:a.name ~data:(Tptr (Some a.elt_type)) with
          | `Ok m -> m
          | _ -> Errors.identifier_already_bound a.name
        in
        { ctx with statics })
;;
