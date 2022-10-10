let rec repeat lexbuf =
  let () = Printf.printf "> %!" in
  try
    let e = Parser.one_expr Lexer.token lexbuf in
    let ty = Typecheck.infer_type Type.empty_context e in
    let redex = Eval.eval e in
    let () = Printf.printf "Type: %s\n%!" (Type.type_as_string ty) in
    let () = Printf.printf "Redex: %s\n%!" (Ast.ast_as_string redex) in
    let () = Printf.printf "Redex type: %s\n%!" 
              (Type.type_as_string 
                (Typecheck.infer_type Type.empty_context redex)) in
    let () = Lexing.flush_input lexbuf in
    repeat lexbuf
  with
  | Lexer.Error msg ->
      let () = Printf.fprintf stderr "%s%!" msg in
      let () = Lexing.flush_input lexbuf in
      repeat lexbuf
  | Parser.Error ->
      let () = 
        Printf.fprintf 
          stderr 
          "At line %d: syntax error.\n%!" 
          (Lexing.lexeme_start_p lexbuf).pos_lnum in
      let () = Lexing.flush_input lexbuf in
      repeat lexbuf
  | Typecheck.Error msg ->
      let () = Printf.fprintf stderr "%s%!" msg in
      let () = Lexing.flush_input lexbuf in
      repeat lexbuf
  | Eval.Error msg ->
      let () = Printf.fprintf stderr "%s%!" msg in
      let () = Lexing.flush_input lexbuf in
      repeat lexbuf
  | Lexer.End_of_file -> Printf.printf "Goodbye!\n%!"

let () =
  let () = Printf.printf "Type expressions followed by \";;\":\n%!" in
  let () = Printf.printf "(CTRL-D (EOF) exits the program)\n%!" in
  repeat (Lexing.from_channel stdin)
