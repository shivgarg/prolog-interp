{
	open Plgparser
}

let charset =['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' '`']*
let varset =['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '`']*
rule plg= parse
| (charset as d){PRED d}
| (varset as d){VARS d}
| [' ']*":-"[' ']* {IF}
| [' ']*","[' ']* {COMMA}
| [' ']*")"[' ']* {RPAREN}
| [' ']*"."[' ']* {STOP}
| ['\n' '\r']+ { NEWLINE }
| [' ']*"("[' ']* {LPAREN}
| [' ']*"\\+"[' ']* {NOT}
| ";" {SEMI}
| eof {raise End_of_file }
| ['\n']*"/*" _* "*/"['\n']*{ plg lexbuf}
