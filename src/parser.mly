%{
  open Lib.Ast
  module Hashtbl = Core.Hashtbl
%}

%token <Lib.Symbol.t> IDENT
%token <int> INT
%token <bool> BOOL
%token RARROW LARROW LRARROW
%token LPAREN RPAREN LANGLE RANGLE LBRACE RBRACE LBRACK RBRACK
%token COMMA DOT COLON EQUAL SEMICOLON
%token ANDAND OROR PLUS MINUS STAR
%token NOT TEST PTR FUN TYPE IF ELSE DEFAULT ALLOC NULL LET RETURN STATIC WITH DO
%token BOOLT UNITT UINTT
%token LEQ GEQ NEQ EQEQ LAND LOR LNOT LSL LSR

%token EOF

%nonassoc IF
%nonassoc ELSE
%left OROR
%left ANDAND
%left LOR
%left LAND
%left EQEQ NEQ
%left LANGLE LEQ RANGLE GEQ
%left LSL LSR
%left PLUS MINUS
%left STAR
%right LNOT
%left TEST NOT
%left DOT

%start <Lib.Ast.decl list> program

%%

program:
| p = list(decl) EOF
  { p }

decl:
| TYPE name = id EQUAL typ = typ SEMICOLON
  { Dtype { name; typ } }
| FUN name = id bound = option(bound) LPAREN args = separated_list(COMMA, param) RPAREN
  RARROW out_type = typ LBRACE body = stmts RETURN out = value SEMICOLON RBRACE
  { Dfunc { name; bound; args; result = (out, out_type); body } }
| STATIC name = id COLON elt_type = typ LBRACK size = INT RBRACK values = option(values) SEMICOLON
  { Dstatic { name; elt_type; size; values } }

values:
| EQUAL LBRACE v = separated_list(COMMA, value) RBRACE
  { v }

bound:
| LBRACK n = id RBRACK
  { Ovar n }
| LBRACK n = id MINUS i = INT RBRACK
  { Ominus (n, i) }
| LBRACK i = INT RBRACK
  { Oconst i }

typ:
| i = id
  { Tvar i }
| UNITT
  { Tunit }
| UINTT
  { Tuint }
| BOOLT
  { Tbool }
| LPAREN t = typ COMMA ts = separated_list(COMMA, typ) RPAREN
  { Tprod (t::ts) }
| PTR LANGLE t = typ RANGLE
  { Tptr (Some t) }

param:
| name = id COLON typ = typ
  { { name; typ } }

stmts:
| s = list(stmt_)
  { Sseq s }

stmt_:
| s = stmt SEMICOLON
| LBRACE s = stmts RBRACE
  { s }
| IF i = id s = stmt_ %prec IF
  { Sif (i, s, Sseq []) }
| IF i = id s1 = stmt_ ELSE s2 = stmt_
  { Sif (i, s1, s2) }
| WITH s1 = stmt_ DO s2 = stmt_
  { Swith (s1, s2) }

stmt:
| LET p = pat LARROW e = exp
  { Sassign (p, e) }
| LET p = pat RARROW e = exp
  { Sunassign (p, e) }
| LET p = pat LARROW e = exp_
  { Sassign (p, e) }
| LET p = pat RARROW e = exp_
  { Sunassign (p, e) }
| i = id LRARROW j = id
  { Sswap (i, j) }
| STAR e = exp LRARROW x = id
  { Smem_swap (e, x) }
| STAR STAR STAR
  { Sdebug }

pat:
| i = id
  { Pid i }
| LPAREN RPAREN
  { Punit }
| b = BOOL
  { Pbool b }
| i = INT
  { Puint i }
| LPAREN ps = separated_nonempty_list(COMMA, pat) RPAREN
  { Pprod ps }
| NULL
  { Pnull }

exp:
| LPAREN e = exp RPAREN
  { e }
| v = value_
  { Eval v }
| e = exp DOT i = INT
  { Eproj (e, i - 1) }
| u = uop e = exp
  { Euop (u, e) }
| e1 = exp b = bop e2 = exp
  { Ebop (b, e1, e2) }
| LPAREN x = exp COMMA xs = separated_list(COMMA, exp) RPAREN
  { Eprod (x::xs) }

exp_:
| ALLOC LANGLE t = typ RANGLE
  { Ealloc t }
| name = id bound = option(bound) LPAREN args = separated_list(COMMA, value) RPAREN
  { Efun { name; bound; args }}

%inline uop:
| NOT
  { Unot }
| TEST
  { Utest }
| LNOT
  { Ulnot }

%inline bop:
| PLUS
  { Bplus }
| MINUS
  { Bminus }
| STAR
  { Btimes }
| ANDAND
  { Band }
| OROR
  { Bor }
| LANGLE
  { Bless }
| RANGLE
  { Bgreater }
| EQEQ
  { Beq }
| NEQ
  { Bneq }
| LEQ
  { Ble }
| GEQ
  { Bge }
| LAND
  { Bland }
| LOR
  { Blor }
| LSL
  { Blsl }
| LSR
  { Blsr }

value:
| v = value_
  { v }
| LPAREN x = value_ COMMA xs = separated_list(COMMA, value_) RPAREN
  { Vprod (x::xs) }

value_:
| i = id
  { Vvar i }
| LPAREN RPAREN
  { Vunit }
| b = BOOL
  { Vbool b }
| i = INT
  { Vnum i }
| NULL
  { Vnull }
| DEFAULT LANGLE t = typ RANGLE
  { Vdefault t }

id:
| i = IDENT
  { i }
