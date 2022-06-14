open Core
open Ast

let rec default = function
  | Tvar _ -> assert false
  | Tunit -> Ir.Vunit
  | Tbool -> Ir.Vbool false
  | Tuint -> Ir.Vnum 0
  | Tprod ts -> Ir.Vprod (List.map ~f:default ts)
  | Tptr _ -> Ir.Vnull
;;

let rec lower_value ctx = function
  | Vvar i -> Ir.Vvar i
  | Vunit -> Ir.Vunit
  | Vnum i -> Ir.Vnum i
  | Vbool b -> Ir.Vbool b
  | Vnull -> Ir.Vnull
  | Vptr (t, p) -> Ir.Vptr (t, p)
  | Vprod vs -> Ir.Vprod (List.map ~f:(lower_value ctx) vs)
  | Vdefault t -> default (Check.normalize_type ctx t)
;;

let rec reverse (stmt : Ir.stmt) : Ir.stmt =
  let open Ir in
  match stmt with
  | Sseq ss -> Sseq (List.rev (List.map ~f:reverse ss))
  | Sassign (x, e) -> Sunassign (x, e)
  | Sunassign (x, e) -> Sassign (x, e)
  | Sswap (x1, x2) -> Sswap (x1, x2)
  | Smem_swap (x1, x2) -> Smem_swap (x1, x2)
  | Sif (x, s1, s2) -> Sif (x, reverse s1, reverse s2)
  | Sdebug -> Sdebug
;;

let flatten (stmt : Ir.stmt) : Ir.stmt =
  let open Ir in
  let rec flatten' stmt =
    match stmt with
    | Sseq ss -> List.concat_map ~f:flatten' ss
    | Sif (x, s1, s2) -> [ Sif (x, Sseq (flatten' s1), Sseq (flatten' s2)) ]
    | s -> [ s ]
  in
  match flatten' stmt with
  | [ s ] -> s
  | ss -> Sseq ss
;;

let rec lower_exp ctx (x : Ir.id) (e : Ast.exp) : Ir.stmt =
  let open Ir in
  match e with
  | Ast.Eval v -> Sassign (x, Eval (lower_value ctx v))
  | Ast.Eprod es ->
    let ys, ss =
      List.map es ~f:(fun e ->
          let y = Symbol.nonce () in
          Vvar y, lower_exp ctx y e)
      |> List.unzip
    in
    Sseq [ Sseq ss; Sassign (x, Eval (Vprod ys)); reverse (Sseq ss) ]
  | Ast.Eproj (v, i) ->
    let y = Symbol.nonce () in
    let s = lower_exp ctx y v in
    Sseq [ s; Sassign (x, Ir.Eproj (y, i)); reverse s ]
  | Ast.Euop (uop, v) ->
    let y = Symbol.nonce () in
    let s = lower_exp ctx y v in
    Sseq [ s; Sassign (x, Euop (uop, y)); reverse s ]
  | Ast.Ebop (bop, v1, v2) ->
    let y1, y2 = Symbol.nonce (), Symbol.nonce () in
    let s1, s2 = lower_exp ctx y1 v1, lower_exp ctx y2 v2 in
    Sseq [ s1; s2; Sassign (x, Ebop (bop, y1, y2)); reverse s2; reverse s1 ]
  | Ast.Ealloc t -> Sassign (x, Ealloc t)
  | Ast.Efun { name; bound; args } ->
    let args = List.map args ~f:(lower_value ctx) in
    Sassign (x, Efun { name; bound; args })
;;

let rec desugar_pat x =
  let open Ir in
  function
  | Ast.Pid i -> [ Sassign (i, Eval (Vvar x)); Sunassign (x, Eval (Vvar i)) ]
  | Ast.Punit -> [ Sunassign (x, Eval Vunit) ]
  | Ast.Pbool b -> [ Sunassign (x, Eval (Vbool b)) ]
  | Ast.Puint n -> [ Sunassign (x, Eval (Vnum n)) ]
  | Ast.Pnull -> [ Sunassign (x, Eval Vnull) ]
  | Ast.Pprod ps ->
    let s, v, rest =
      List.mapi ps ~f:(fun i -> function
        | Pid y -> Sassign (y, Eproj (x, i)), Vvar y, []
        | p ->
          let y = Symbol.nonce () in
          Sassign (y, Ir.Eproj (x, i)), Vvar y, desugar_pat y p)
      |> List.unzip3
    in
    s @ (Sunassign (x, Eval (Vprod v)) :: List.concat rest)
;;

let rec lower_stmt ctx (stmt : Ast.stmt) : Ir.stmt =
  match stmt with
  | Sseq ss -> Ir.Sseq (List.map ss ~f:(lower_stmt ctx))
  | Sassign (Pid i, e) -> lower_exp ctx i e
  | Sassign (pat, e) ->
    let x = Symbol.nonce () in
    let s = lower_exp ctx x e in
    Ir.Sseq [ s; Ir.Sseq (desugar_pat x pat) ] |> flatten
  | Sunassign (x, e) -> reverse (lower_stmt ctx (Sassign (x, e)))
  | Sswap (x1, x2) -> Ir.Sswap (x1, x2)
  | Smem_swap (Eval (Vvar x1), x2) -> Ir.Smem_swap (x1, x2)
  | Smem_swap (e1, x2) ->
    let x = Symbol.nonce () in
    let s = lower_exp ctx x e1 in
    Ir.Sseq [ s; Ir.Smem_swap (x, x2); reverse s ]
  | Sif (x, s1, s2) -> Ir.Sif (x, lower_stmt ctx s1, lower_stmt ctx s2)
  | Sdebug -> Ir.Sdebug
  | Swith (s1, s2) ->
    let s1 = lower_stmt ctx s1 in
    Ir.Sseq [ s1; lower_stmt ctx s2; reverse s1 ]
;;

let lower_func ctx ({ name; bound; args; result; body } : Ast.func) : Ir.func =
  let body = lower_stmt ctx body |> flatten in
  let result, typ = result in
  match result with
  | Vvar out -> { name; bound; args; result = { name = out; typ }; body }
  | result ->
    let out = Symbol.nonce () in
    let body = Ir.Sseq [ body; Ir.Sassign (out, Ir.Eval (lower_value ctx result)) ] in
    { name; bound; args; result = { name = out; typ }; body }
;;

let lower_static ctx ({ name; elt_type; size; values } : Ast.static) : Ir.static =
  { name; elt_type; size; values = Option.map ~f:(List.map ~f:(lower_value ctx)) values }
;;

let lower_decl ctx (decl : Ast.decl) : Ir.decl =
  match decl with
  | Dstatic s -> Ir.Dstatic (lower_static ctx s)
  | Dtype t -> Ir.Dtype t
  | Dfunc f -> Ir.Dfunc (lower_func ctx f)
;;
