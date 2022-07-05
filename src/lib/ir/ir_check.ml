open Core
module IdMap = Symbol.Map
module Ir = Ir_

module Context = struct
  type t =
    { locals : Ir.typ IdMap.t
    ; funcs : Ir.func IdMap.t
    }

  let empty (funcs : Ir.func list) : t =
    { locals = IdMap.empty
    ; funcs =
        List.fold
          ~init:IdMap.empty
          ~f:(fun acc f -> IdMap.add_exn acc ~key:f.name ~data:f)
          funcs
    }
  ;;

  let add_local ctx key data = { ctx with locals = IdMap.add_exn ctx.locals ~key ~data }
  let get_local ctx key = IdMap.find_exn ctx.locals key
  let remove_local ctx key = { ctx with locals = IdMap.remove ctx.locals key }
end

let rec synth_value ctx = function
  | Ir.Vvar i -> Context.get_local ctx i
  | Vunit -> Ast.Tunit
  | Vnum _ -> Tuint
  | Vbool _ -> Tbool
  | Vnull -> Tptr None
  | Vptr (t, _) -> Tptr (Some t)
  | Vprod vs -> Tprod (List.map ~f:(synth_value ctx) vs)
;;

let synth_exp ctx = function
  | Ir.Eval v -> synth_value ctx v
  | Eproj (v, i) ->
    (match Context.get_local ctx v with
    | Tprod ts -> List.nth_exn ts i
    | _ -> assert false)
  | Euop (uop, v) ->
    (match uop, Context.get_local ctx v with
    | Unot, Tbool | Utest, Tuint | Utest, Tptr _ -> Tbool
    | Ulnot, Tuint -> Tuint
    | _ -> assert false)
  | Ebop (bop, x, y) ->
    let t1 = Context.get_local ctx x in
    let t2 = Context.get_local ctx y in
    (match bop, t1, t2 with
    | (Band | Bor | Beq | Bneq | Bless | Bgreater | Ble | Bge), Tbool, Tbool -> Tbool
    | (Beq | Bneq | Bless | Bgreater | Ble | Bge), Tuint, Tuint -> Tbool
    | (Beq | Bneq), Tptr _, Tptr _ -> Tbool
    | (Bplus | Bminus | Btimes), Tuint, Tuint -> Tuint
    | (Bplus | Bminus), Tptr t, Tuint -> Tptr t
    | (Bland | Blor | Blsl | Blsr), Tuint, Tuint -> Tuint
    | _ -> assert false)
  | Ealloc t -> Tptr (Some t)
  | Efun f -> (IdMap.find_exn ctx.funcs f.name).result.typ
;;
