open Core
open Format
open Lib

exception Error of string

let print_position _ lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse parsing_fun lexing_fun source_name =
  let ic = In_channel.create source_name in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p
    <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = source_name };
  try
    let p = parsing_fun lexing_fun lexbuf in
    In_channel.close ic;
    p
  with
  | Lexer.Lexical_error err ->
    In_channel.close ic;
    raise (Error (sprintf "%a: %s\n" print_position lexbuf err))
  | Parser.Error ->
    In_channel.close ic;
    raise (Error (sprintf "%a: Syntax error.\n" print_position lexbuf))
;;

let decls = ref []

let load file =
  let d = parse Parser.program (Lexer.token ()) file in
  decls := !decls @ d
;;

let gen_ir decls =
  let ctx =
    try Ast.Check.check_decls ~init:Ast.Check.Context.empty decls with
    | Errors.StaticError e ->
      raise (Error ("Static error: " ^ Errors.static_error_name e ^ "."))
  in
  if !Args.verbose then Format.eprintf "Lowering...\n%!";
  ctx, Ir.Lower.lower_decls ctx decls
;;

let inline ir =
  if !Args.verbose then Format.eprintf "Inlining...\n%!";
  let bounds =
    String.Table.to_alist Args.bounds
    |> List.map ~f:(fun (f, b) -> Symbol.get_sym f, b)
    |> Symbol.Map.of_alist_exn
  in
  Ir.Inline.inline ~bounds ir |> Ir.Linear.linearize
;;

let interp ctx decls =
  if !Args.verbose then Format.eprintf "Interpreting...\n%!";
  let state, ret =
    let mem_size = !Args.mem_size in
    try Interp.interp ~mem_size ctx decls with
    | Errors.RuntimeError (s, dump) ->
      dump ();
      raise (Error ("Runtime error: " ^ s ^ "."))
    | Interp.OutOfMemory -> raise (Error "Runtime error: Out of memory.")
  in
  if !Args.print
  then (
    Option.iter ~f:(fun v -> Format.printf "Output: %s\n" (Ir.Pp.show_value v)) ret;
    Interp.State.debug state)
;;

let interp_lir lir =
  if !Args.verbose then Format.eprintf "Interpreting...\n%!";
  let state =
    let mem_size = !Args.mem_size in
    try Lir.Sim.interp ~mem_size lir with
    | Errors.RuntimeError (s, dump) ->
      dump ();
      raise (Error ("Runtime error: " ^ s ^ "."))
    | Interp.OutOfMemory -> raise (Error "Runtime error: Out of memory.")
  in
  if !Args.print then Lir.Sim.State.debug state
;;

let interp_circuit circuit =
  if !Args.verbose then Format.eprintf "Interpreting...\n%!";
  let state =
    let mem_size = !Args.mem_size in
    try Circuit.Sim.interp ~mem_size circuit with
    | Errors.RuntimeError (s, dump) ->
      dump ();
      raise (Error ("Runtime error: " ^ s ^ "."))
    | Interp.OutOfMemory -> raise (Error "Runtime error: Out of memory.")
  in
  if !Args.print then Circuit.Sim.State.debug state
;;

let interp_prim circuit =
  if !Args.verbose then Format.eprintf "Interpreting...\n%!";
  let state =
    try Prim_circuit.Sim.interp circuit with
    | Errors.RuntimeError (s, dump) ->
      dump ();
      raise (Error ("Runtime error: " ^ s ^ "."))
    | Interp.OutOfMemory -> raise (Error "Runtime error: Out of memory.")
  in
  if !Args.print then print_endline @@ Prim_circuit.Sim.show_reg state
;;

let gen_prim circuit =
  if !Args.verbose then Format.eprintf "Instantiating primitives...\n%!";
  let prim_circuit =
    List.map circuit ~f:(fun m ->
        Prim_circuit.Gen.lower_module m ~num_cells:!Args.mem_size)
  in
  if !Args.interp_prim then interp_prim prim_circuit;
  if !Args.print
  then List.iter prim_circuit ~f:(fun m -> print_endline @@ Prim_circuit.Pp.show_modul m);
  if !Args.verbose
  then List.iter prim_circuit ~f:(fun m -> print_endline @@ Prim_circuit.Pp.show_stats m)
;;

let compile funcs =
  if !Args.verbose then Format.eprintf "Lowering...\n%!";
  let lir = List.map funcs ~f:(Lir.Lower.lower_func funcs) in
  if !Args.verbose then List.iter lir ~f:(fun m -> print_endline @@ Lir.Pp.show_stats m);
  if !Args.print then List.iter lir ~f:(fun m -> print_endline @@ Lir.Pp.show_module m);
  if !Args.interp_lir then interp_lir lir;
  if !Args.verbose then Format.eprintf "Generating circuit...\n%!";
  let circuit = List.map lir ~f:Circuit.Gen.(lower_module ~word_size:!Args.word_size) in
  if !Args.interp_circuit then interp_circuit circuit;
  if !Args.verbose
  then List.iter circuit ~f:(fun m -> print_endline @@ Circuit.Pp.show_stats m);
  if not !Args.no_prim then gen_prim circuit
;;

let run () =
  let ctx, ir = gen_ir !decls in
  match !Args.inline, !Args.interp, !Args.compile with
  | true, true, true ->
    let ir = inline ir in
    let ir' = List.map ~f:(fun f -> Ir.Dfunc f) ir in
    interp ctx ir';
    compile ir
  | _, true, true ->
    interp ctx ir;
    compile @@ inline ir
  | _, false, true -> compile @@ inline ir
  | true, true, false ->
    let ir = inline ir in
    let ir' = List.map ~f:(fun f -> Ir.Dfunc f) ir in
    interp ctx ir'
  | false, true, false -> interp ctx ir
  | _, false, false -> ()
;;

let () =
  let desc =
    Format.sprintf
      "The interpreter for the Tower quantum programming language.\n\
       Usage: %s <options> [program] \n\
       Options are:"
      (Sys.get_argv ()).(0)
  in
  try
    Arg.parse Args.speclist load desc;
    run ()
  with
  | Error s ->
    fprintf std_formatter "%s" s;
    exit 1
;;
