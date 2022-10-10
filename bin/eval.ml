
open Ast

exception Error of string

let rec eval = function
  | Bool(b) -> Bool(b)
  | Var(v) -> Var(v)
  | Ite(c, e1, e2) ->
      (match eval c with
      | Bool(true) -> eval e1
      | Bool(false) -> eval e2
      | _ -> Ite(c, e1, e2))
  | Abs(arg, body) -> Abs(arg, body)
  | Let(var, init, body) -> 
      let redex = eval init in
      eval (subst redex (arg_name var) (eval body))
  | App(e1, e2) ->
      let redex = eval e1 in
      (match redex with 
      | Abs(arg, body) ->
          eval (subst (eval e2) (arg_name arg) (eval body))
      | _ -> App(redex, eval e2))
  | Annotated_expr(e1, _) -> eval e1
