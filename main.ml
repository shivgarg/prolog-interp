(* file: main.ml *)
(* Assumes the parser file is "rtcalc.mly" and the lexer file is "lexer.mll". *)
open Printf

let main () =
try
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
while true do
Plgparser.input Plglex.plg lexbuf
done
with End_of_file -> (
	printf "File Compiled\n";
	printf "Entering Interactive mode\n";printf "?-  ";flush stdout;

	let lexbuf2 = Lexing.from_channel stdin in
 try while true do try 
				Plgparser.interactive Plglex.plg lexbuf2 
					with Parsing.Parse_error -> printf "Not a Valid query\n";flush stdout;
				
done with End_of_file -> (printf "\n";exit 0)
)
let _ = Printexc.print main ()
