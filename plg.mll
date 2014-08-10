{
	open Plgparser
}

let charset =['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' '`']*
let varset =['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '`']*
rule plg= parse
| (charset as d){PRED d}
| (varset as d){VARS d}
| ":-" {IF}
| [' ']*","[' ']* {COMMA}
| [' ']*")"[' ']* {RPAREN}
| [' ']*"."[' ']* {STOP}
