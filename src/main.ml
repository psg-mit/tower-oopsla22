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
let mem_size = 16

let load file =
  let d = parse Parser.program (Lexer.token ()) file in
  decls := !decls @ d
;;

let run () =
  let decls = !decls in
  let ctx =
    try Check.check_decls ~init:Check.Context.empty decls with
    | Errors.StaticError e ->
      raise (Error ("Static error: " ^ Errors.static_error_name e ^ "."))
  in
  let lower = List.map ~f:(Ir_lower.lower_decl ctx) decls in
  let state, ret =
    try Interp.interp ~mem_size ctx lower with
    | Errors.RuntimeError (s, dump) ->
      dump ();
      raise (Error ("Runtime error: " ^ s ^ "."))
    | Interp.OutOfMemory -> raise (Error "Runtime error: Out of memory.")
  in
  Option.iter ~f:(fun v -> Format.printf "Output: %s\n" (Ir.Pp.show_value v)) ret;
  if not (Interp.State.check_leaks state)
  then
    Format.eprintf
      "Warning: memory may not have been reset to initial state at program termination.\n";
  if !Args.verbose then Interp.State.debug state
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
