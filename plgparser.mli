type token =
  | VARS of (string)
  | PRED of (string)
  | RPAREN
  | LPAREN
  | COMMA
  | STOP
  | IF
  | NOT
  | SEMI
  | NEWLINE
  | END

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
val interactive :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
