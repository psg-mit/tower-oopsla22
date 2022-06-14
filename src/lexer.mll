{
  open Core
  open Lib
  open Parser
  exception Lexical_error of string

  let keyword_table =
    let tbl = Hashtbl.create (module String) in
    begin
      List.iter ~f:(fun (key, data) -> Hashtbl.add_exn tbl ~key ~data)
	[   ("not", NOT);
        ("test", TEST);
        ("ptr", PTR);
        ("fun", FUN);
        ("type", TYPE);
        ("if", IF);
        ("else", ELSE);
        ("bool", BOOLT);
        ("unit", UNITT);
        ("uint", UINTT);
        ("true", BOOL true);
        ("false", BOOL false);
        ("default", DEFAULT);
        ("alloc", ALLOC);
        ("null", NULL);
        ("let", LET);
        ("return", RETURN);
        ("static", STATIC);
        ("land", LAND);
        ("lor", LOR);
        ("lnot", LNOT);
        ("lsl", LSL);
        ("lsr", LSR);
        ("with", WITH);
        ("do", DO);
	]; tbl
    end
}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['A'-'Z' 'a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let tick = '\''

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* (frac exp? | exp)
let int = '0' | ['1'-'9'] digit*
let hex = "0x" digit+
let bin = "0b" ['0'-'1']+

rule token sbuff = parse
| eof { EOF }
| "->" { RARROW }
| "<-" { LARROW }
| "<->" { LRARROW }
| "(" { LPAREN }
| ")" { RPAREN }
| "<" { LANGLE }
| ">" { RANGLE }
| "{" { LBRACE }
| "}" { RBRACE }
| "[" { LBRACK }
| "]" { RBRACK }
| "&&" { ANDAND }
| "||" { OROR }
| "+" { PLUS }
| "-" { MINUS }
| "*" { STAR }
| "." { DOT }
| "," { COMMA }
| ":" { COLON }
| ";" { SEMICOLON }
| "=" { EQUAL }
| "!=" { NEQ }
| "==" { EQEQ }
| "<=" { LEQ }
| ">=" { GEQ }
| [' ' '\t']
    { token sbuff lexbuf }
| newline
    { Lexing.new_line lexbuf; token sbuff lexbuf }
| letter identchar*
    { let s = Lexing.lexeme lexbuf in
      match Hashtbl.find keyword_table s
        with Some s -> s | None -> IDENT (Symbol.get_sym s) }
| int
    { let s = Lexing.lexeme lexbuf in INT (Int.of_string s) }
| hex
    { let s = Lexing.lexeme lexbuf in INT (Int.of_string s) }
| bin
    { let s = Lexing.lexeme lexbuf in INT (Int.of_string s) }
| "/*"
    { comment 1 lexbuf; token sbuff lexbuf }
| _
    { raise (Lexical_error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and comment cpt = parse
  | "/*"
      { comment (cpt + 1) lexbuf }
  | "*/"
      { if cpt > 1 then comment (cpt - 1) lexbuf }
  | eof
      { raise (Lexical_error "Unterminated comment.\n") }
  | newline
      { Lexing.new_line lexbuf; comment cpt lexbuf }
  | _
      { comment cpt lexbuf }
