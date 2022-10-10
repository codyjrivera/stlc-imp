
open Type

type expr = 
  | Bool of bool
  | Var of string
  | Ite of expr * expr * expr
  | Abs of arg * expr
  | Let of arg * expr * expr
  | App of expr * expr
  | Annotated_expr of expr * type_expr

and arg =
  | Id of string
  | Annotated_id of string * type_expr

let arg_name = function
  | Id(n) -> n
  | Annotated_id(n, _) -> n

let rec desugar_lambda f e =
  match f with
  | [] -> e
  | (h :: t) -> Abs(h, desugar_lambda t e)

and ast_as_string = function
  | Bool(b) -> string_of_bool b
  | Var(v) -> v
  | Ite(c, e1, e2) -> 
      "if " ^ ast_as_string c ^ " then " 
      ^ ast_as_string e1 ^ " else " ^ ast_as_string e2
  | Abs(arg, body) -> 
      "lambda (" ^ arg_as_string arg ^ ") => "
      ^ ast_as_string body
  | Let(var, init, body) -> 
      "let " ^ arg_as_string var ^ " = " ^ ast_as_string init
      ^ " in " ^ ast_as_string body
  | App(e1, e2) ->
      "((" ^ ast_as_string e1 ^ ") " ^ ast_as_string e2 ^ ")" 
  | Annotated_expr(e1, ty) ->
      ast_as_string e1 ^ " : " ^ type_as_string ty

and arg_as_string = function
  | Id(a) -> a
  | Annotated_id(a, ty) -> a ^ " : " ^ type_as_string ty

and subst e v = function
  | Bool(b) -> Bool(b)
  | Var(v') -> 
    if String.equal v v' then e else Var v'
  | Ite(c, e1, e2) -> Ite(subst e v c, subst e v e1, subst e v e2)
  | Abs(arg, body) -> 
      if String.equal (arg_name arg) v then
        Abs(arg, body) (* Avoid rebinding *)
      else
        Abs(arg, subst e v body)
  | Let(var, init, body) -> 
    if String.equal (arg_name var) v then
      Let(var, subst e v init, body) (* Avoid rebinding *)
    else
      Let(var, subst e v init, subst e v body)
  | App(e1, e2) -> App(subst e v e1, subst e v e2)
  | Annotated_expr(e1, ty) -> Annotated_expr(subst e v e1, ty)
