{
  open Parser

  exception Error of string
  exception End_of_file
}

rule token = parse
| [' ' '\012' '\t' '\r']
    { token lexbuf }
| '\n'
    { let () = Lexing.new_line lexbuf in
      token lexbuf }
| "true"
    { TRUE }
| "false"
    { FALSE }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| ':'
    { COLON }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "lambda"
    { LAMBDA }
| "=>"
    { DARROW }
| "let"
    { LET }
| '='
    { EQUALS }
| "in"
    { IN }
| "->"
    { ARROW }
| ';'
    { SEMICOLON }
| ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as id
    { ID(id) }
| '\''['A'-'Z''a'-'z''0'-'9''_']+ as tv
    { TVAR(tv) }
| "(*"
    { comment 0 lexbuf }
| eof
    { raise End_of_file }
| _
    { raise (Error (Printf.sprintf 
                    "At line %d: unexpected character.\n" 
                    (Lexing.lexeme_start_p lexbuf).pos_lnum)) }

and comment level = parse
| '\n'
    { let () = Lexing.new_line lexbuf in
      comment level lexbuf }
| "(*"
    { comment (level + 1) lexbuf }
| "*)"
    { if level == 0 then token lexbuf
      else comment (level - 1) lexbuf }
| eof 
    { raise (Error (Printf.sprintf 
                    "At line %d: unexpected EOF in comment.\n" 
                    (Lexing.lexeme_start_p lexbuf).pos_lnum)) }
| _
    { comment level lexbuf }
