# 1 "plglex.mll"
 
	open Plgparser

# 6 "plglex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\000\000\245\255\246\255\000\000\001\000\001\000\002\000\
    \002\000\003\000\004\000\000\000\091\000\090\000\165\000\005\000\
    \006\000\049\000\082\000\051\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\007\000\006\000\006\000\
    \005\000\004\000\003\000\255\255\255\255\001\000\000\000\002\000\
    \008\000\255\255\255\255\011\000";
  Lexing.lex_default = 
   "\255\255\255\255\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\017\000\017\000\017\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\007\000\006\000\007\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \012\000\005\000\008\000\009\000\010\000\015\000\016\000\000\000\
    \005\000\009\000\017\000\016\000\010\000\015\000\008\000\001\000\
    \000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\011\000\003\000\000\000\019\000\000\000\000\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\018\000\004\000\018\000\000\000\000\000\
    \000\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\012\000\018\000\000\000\000\000\000\000\
    \000\000\019\000\000\000\005\000\009\000\000\000\000\000\010\000\
    \000\000\008\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\011\000\000\000\000\000\
    \000\000\000\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\000\000\000\000\004\000\
    \000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \002\000\000\000\000\000\000\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\006\000\007\000\000\000\006\000\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\005\000\008\000\009\000\010\000\015\000\016\000\255\255\
    \000\000\000\000\001\000\004\000\000\000\011\000\000\000\000\000\
    \255\255\007\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\019\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\017\000\000\000\019\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\012\000\018\000\255\255\255\255\255\255\
    \255\255\018\000\255\255\012\000\012\000\255\255\255\255\012\000\
    \255\255\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\255\255\012\000\255\255\255\255\
    \255\255\255\255\255\255\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\255\255\255\255\012\000\
    \255\255\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \000\000\255\255\255\255\255\255\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\017\000\255\255\019\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\018\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec plg lexbuf =
    __ocaml_lex_plg_rec lexbuf 0
and __ocaml_lex_plg_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 8 "plglex.mll"
              d
# 150 "plglex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 8 "plglex.mll"
                (PRED d)
# 154 "plglex.ml"

  | 1 ->
let
# 9 "plglex.mll"
             d
# 160 "plglex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 9 "plglex.mll"
               (VARS d)
# 164 "plglex.ml"

  | 2 ->
# 10 "plglex.mll"
                   (IF)
# 169 "plglex.ml"

  | 3 ->
# 11 "plglex.mll"
                  (COMMA)
# 174 "plglex.ml"

  | 4 ->
# 12 "plglex.mll"
                  (RPAREN)
# 179 "plglex.ml"

  | 5 ->
# 13 "plglex.mll"
                  (STOP)
# 184 "plglex.ml"

  | 6 ->
# 14 "plglex.mll"
               ( NEWLINE )
# 189 "plglex.ml"

  | 7 ->
# 15 "plglex.mll"
                  (LPAREN)
# 194 "plglex.ml"

  | 8 ->
# 16 "plglex.mll"
                    (NOT)
# 199 "plglex.ml"

  | 9 ->
# 17 "plglex.mll"
      (SEMI)
# 204 "plglex.ml"

  | 10 ->
# 18 "plglex.mll"
      (raise End_of_file )
# 209 "plglex.ml"

  | 11 ->
# 19 "plglex.mll"
                            ( plg lexbuf)
# 214 "plglex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_plg_rec lexbuf __ocaml_lex_state

;;

