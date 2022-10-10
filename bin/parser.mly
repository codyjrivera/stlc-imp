%{
    open Ast
    open Type
    open List
%}

%token <string> ID
%token TRUE FALSE
%token LPAREN RPAREN COLON

%token IF THEN ELSE
%token LAMBDA DARROW
%token LET IN

%token <string> TVAR
%token ARROW

%token EQUALS

%token SEMICOLON

(* Precedence, high to low *)
%left ARROW

%start<expr> one_expr

%%

one_expr:
| e=expr SEMICOLON SEMICOLON
    { e }

expr:
| e=expr COLON t=type_expr  (* explicit typing *)
    { Annotated_expr(e, t) }
| e=expr_1
    { e }

expr_1:
| IF t=expr_1 THEN e1=expr_1 ELSE e2=expr_1  (* conditional *)
    { Ite(t, e1, e2) }
| LAMBDA f=formals DARROW e=expr_1   (* abstraction *)
    { desugar_lambda f e }
| LET f=full_formal EQUALS i=expr_1 IN e=expr_1
    { Let(f, i, e) }
| e=expr_2
    { e }

expr_2:
| e1=expr_2 e2=expr_3  (* application *)
    { App(e1, e2) }
| e=expr_3
    { e }

expr_3:
| TRUE
    { Bool(true) }
| FALSE
    { Bool(false) }
| i=ID
    { Var(i) }
| LPAREN e=expr RPAREN
    { e }

formals:
| f=ids
    { rev f }
| i=ID COLON t=type_expr
    { Annotated_id(i, t) :: [] }
| f=full_formals
    { rev f }

ids: (* Returns reversed list *)
| i=ID
    { Id(i) :: [] }
| l=ids i=ID
    { Id(i) :: l }

full_formals: (* Returns reversed list *)
| LPAREN f=full_formal RPAREN
    { f :: [] }
| l=full_formals LPAREN f=full_formal RPAREN
    { f :: l }

full_formal:
| i=ID COLON t=type_expr
    { Annotated_id(i, t) }
| i=ID
    { Id(i) }

type_expr:
| i=ID
    { Typename(i) }
| v=TVAR
    { Typevar(v) }
| e1=type_expr ARROW e2=type_expr  (* arrow type *)
    { Arrow(e1, e2) }
