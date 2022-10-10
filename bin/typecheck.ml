
open Ast
open Type

exception Error of string

let rec infer_type ctx = function
  | Bool(_) -> Typename("Bool")
  | Var(v) -> 
      (match lookup v ctx with
      | Some(Judgement(_, ty)) -> ty
      | None -> raise (Error ("Unbound identifier: " ^ v ^ "\n")))
  | Ite(c, e1, e2) ->
      let is_bool = check_type ctx c (Typename("Bool")) in
      if is_bool then
        let t1 = infer_type ctx e1 in
        let t2 = infer_type ctx e2 in
        if type_equiv t1 t2 then
          t1
        else
          raise (Error "If statements don't have same type\n")
      else
        raise (Error "Condition not boolean\n")
  | Abs(arg, body) ->
      (match arg with
      | Annotated_id(a, ta) ->
          let tb = infer_type (add (Judgement(a, ta)) ctx) body in
          Arrow(ta, tb)
      | _ -> raise (Error "Arguments must have a type\n"))
  | Let(var, init, body) ->
      (match var with
      | Annotated_id(v, tv) ->
          let ti = infer_type ctx init in
          if type_equiv ti tv then
            infer_type (add (Judgement(v, tv)) ctx) body
          else
            raise (Error "Let binding type doesn't match\n")
      | Id(v) ->
          let tv = infer_type ctx init in
          infer_type (add (Judgement(v, tv)) ctx) body)
  | App(e1, e2) -> 
      (match infer_type ctx e1 with
      | Arrow(t1, t2) ->
          let ta = infer_type ctx e2 in
          if type_equiv t1 ta then
            t2
          else
            raise (Error "Improper argument type\n")
      | _ -> raise (Error "Bad application\n"))
  | Annotated_expr(e, ty) ->
      let is_ty = check_type ctx e ty in
      if is_ty then
        ty
      else
        raise (Error "Annotation inconsistent with real type\n")

and check_type ctx e ty =
  type_equiv (infer_type ctx e) ty
