
type type_expr =
  | Typevar of string
  | Typename of string
  | Arrow of type_expr * type_expr

let rec type_as_string = function
  | Typevar(v) -> v
  | Typename(t) -> t
  | Arrow(t1, t2) -> type_as_string_parens t1 ^ " -> " ^ type_as_string t2

and type_as_string_parens = function
  | Typevar(v) -> v
  | Typename(t) -> t
  | Arrow(t1, t2) -> "(" ^ type_as_string_parens t1 ^ " -> " ^ type_as_string t2 ^ ")"

let rec type_equiv t1 t2 = 
  match t1, t2 with
  | Typevar(v1), Typevar(v2) -> String.equal v1 v2
  | Typename(n1), Typename(n2) -> String.equal n1 n2
  | Arrow(a1a, a1b), Arrow(a2a, a2b) ->
      type_equiv a1a a2a && type_equiv a1b a2b
  | _, _ -> false

type context_item = 
  | Judgement of string * type_expr
type context = context_item list

let rec lookup var = function
  | [] -> None
  | (Judgement(id, ty) :: t) ->
      if String.equal id var then 
        Some(Judgement(id, ty))
      else
        lookup var t

let add j ctx = j :: ctx

let empty_context = []
