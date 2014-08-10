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

open Parsing;;
let _ = parse_error;;
# 2 "plgparser.mly"
open Printf

type atom =  Var of string| Sym of string |B of string*(atom list);;

type code = Fact of int*string *(atom list) | Rule of int*atom * (code list);;

let symb = Hashtbl.create 1;;

let prog = Hashtbl.create 1;; 

exception Not_WellFormed;;

let rec ui  a = function (Sym x) -> (Sym x)
				| (Var y) -> if Hashtbl.mem a (Var y) = true then Hashtbl.find a (Var y) else (Var y)
				| B(x,y) -> B(x,(List.map (ui a) y));;

let queryans = ref [];;
(*
let p f = 
	let Fact(a,b,c) = f in 
			printf "%s " b ; 
			for o = 0 to (List.length c)-1 do 
				match List.nth c o with 
				 Var x ->	printf "%s ," x
				 | Sym x -> printf "%s ," x
			done ;
			printf "\n";;*)

let x1 = ref 0;;

(*let rec fa a = let l = Hashtbl.find prog "brotherinlaw" in 
let Rule(c,b,m) = List.nth l a in for n=0 to (List.length m)-1 do
										p (List.nth m n); printf "|||" ;done; printf "\n";flush stdout;fa (a+1);;*)
exception Not_Defined ;;

exception No_Match ;;

let equality a x = if x=a then true else false;;

let rec app a b = match b with 
		[] -> a
		| x::xs -> try let f = List.find (equality x) a in app a xs with Not_found -> app (a@[x]) xs
		;;

let rec variable term = 
	match term with 
	Sym b -> []
	| Var x -> [Var x]
	| B(a,b) -> List.fold_right app (List.map variable b) []
	;;

let rec qwerty t = 
	match t with 
	| [x] -> (match x with 
			Fact(a,b,c) -> List.fold_right app (List.map variable c) []
			| Rule(z,B(b,d),c) -> app (List.fold_right app (List.map variable d) []) (qwerty c))
	| x::xs -> match x with 
			Fact(a,b,c) -> app (List.fold_right app (List.map variable c) []) (qwerty xs)
			| Rule(a,B(b,d),c) -> app (app (List.fold_right app (List.map variable d) []) (qwerty c)) (qwerty xs)
	;;

let varsub t id = 
	let a = ref (Hashtbl.create 1) in 
		for i = 0 to (List.length t) -1 do
			Hashtbl.add (!a) (List.nth t i) (Var (String.concat "" ["z";string_of_int(id+i)]))
		done ;

		ui (!a);;

let rec substi f t = match t with 
		(Sym x) -> 	 (Sym x)
		| (Var p) ->  f (Var p)
		| B(x,y)  -> B(x,List.map (substi f) y)
		;;   

let rec instrsubst f t = match t with 
		Fact(a,b,c) -> Fact(a,b,((List.map (substi f)) c))
		| Rule(a,B(b,d),c) -> Rule(a,(substi f (B(b,d))),(List.map (instrsubst f) c));;

let compose f g = function x -> g(f(x));;

let rec matchterms subst x z= let t3= substi subst x in let t4 = substi subst z in match (t3,t4) with 
		(Sym a, Sym c) ->
			if a = c then subst
			else raise No_Match 
		| (Sym a , Var c) -> 
			let z = Hashtbl.create 1 in let () = Hashtbl.add z (Var c) (Sym a) in compose subst (ui z)
		| (Var a, Sym b) ->
			let z = Hashtbl.create 1 in let () = Hashtbl.add z (Var a) (Sym b) in compose subst (ui z)
		| (Sym a , B(b,c)) ->
				raise No_Match
		| (Var a, Var b) ->
			let z = Hashtbl.create 1 in let () = Hashtbl.add z (Var a) (Var b) in compose subst (ui z)
		| (Var a, B(b,c)) ->
			let z = Hashtbl.create 1 in let () = Hashtbl.add z (Var a) (B(b,c)) in compose subst (ui z)
		| (B(b,c), Sym a) ->
				raise No_Match
		| (B (b,c), Var a) ->
			let z = Hashtbl.create 1 in let () = Hashtbl.add z (Var a) (B(b,c)) in compose subst (ui z)
		| (B(x,y),B(w,z)) ->if x=w then 
					let rec dp p a b = match (a,b) with 
						([],[]) -> p
						| (x::xs,y::ys) -> let g = matchterms p x y in dp g xs ys 
						in 
						let n = dp subst y z in n else raise No_Match
;;

let mgu param1 param2 subst = 
		matchterms subst param1 param2 ;;
(*)
let rec fillst othergoals param factlist subst c index=
	(*printf "%d\t%d\n" index (List.length factlist);flush stdout;*)
	  if index = (List.length factlist) then c else 
	  try let t = List.nth factlist index in 
	  	match t with 
			Fact(x,y,z) -> let f = mgu (B("-1",param)) (B("-1",z)) (subst) in 
								let ogoals = (List.map (instrsubst f) othergoals) in 
									fillst othergoals param factlist subst ([(ogoals,f)]@c) (index+1)
			| Rule(x,y,z) -> 	
								let B(q,g)=y in 
								let f = instrsubst (varsub (qwerty [Rule(x,y,z)]) !x1) (Rule(x,y,z)) in x1:=!x1+(List.length (qwerty [Rule(x,y,z)] ));
								let Rule(i,B(jk,o),p) = f in let gh = matchterms (ui (Hashtbl.create 1)) (B(jk,o)) (B(jk,param)) in 
									fillst othergoals param factlist subst ([(((List.map (instrsubst gh) p)@othergoals),subst)]@c) (index+1)
			
	with  No_Match -> fillst othergoals param factlist subst (c) (index+1);;

let unify goal other_goals subst = 
		let Fact(a,b,c) =goal in  
				if Hashtbl.mem symb b = false then 
					raise Not_Defined
				else if a = 0 then fillst other_goals c (Hashtbl.find prog b) subst [] 0
				else let bv = interp [([Fact(0,b,c)],(ui (Hashtbl.create 1)))] [] in 
					if (List.length bv) <> 0 then [] else [(other_goals,subst)];;*)



let rec interpneg stackdfs answers = 
		if (List.length stackdfs = 0 ) then answers else  
		let (a,b) = (List.nth stackdfs 0) in 
			if (List.length a)=0 then 
				[b]
			else  
				let eval list_of_goals subst_list=
					let unify goal other_goals subst = 
						let Fact(a,b,c) =goal in  
							if Hashtbl.mem symb b = false then 
								raise Not_Defined
							else 
								if a = 0 then 
									let rec fillst othergoals param factlist subst c index=
									  if index = (List.length factlist) then c else 
	  									try let t = List.nth factlist index in 
	  										match t with 
												Fact(x,y,z) -> let f = mgu (B("-1",param)) (B("-1",z)) (subst) in 
													let ogoals = (List.map (instrsubst f) othergoals) in 
									fillst othergoals param factlist subst ([(ogoals,f)]@c) (index+1)
			| Rule(x,y,z) -> 	
								let B(q,g)=y in 
								let f = instrsubst (varsub (qwerty [Rule(x,y,z)]) !x1) (Rule(x,y,z)) in x1:=!x1+(List.length (qwerty [Rule(x,y,z)] ));
								let Rule(i,B(jk,o),p) = f in let gh = matchterms (ui (Hashtbl.create 1)) (B(jk,o)) (B(jk,param)) in 
									fillst othergoals param factlist subst ([(((List.map (instrsubst gh) p)@othergoals),subst)]@c) (index+1)
			
	with  No_Match -> fillst othergoals param factlist subst (c) (index+1)
								in fillst other_goals c (Hashtbl.find prog b) subst [] 0
								else 
									let bv = interpneg [([Fact(0,b,c)],(ui (Hashtbl.create 1)))] [] in 
										if (List.length bv) <> 0 then [] 
										else [(other_goals,subst)] in 
					unify (List.nth list_of_goals 0) (List.tl list_of_goals) subst_list in 
					let c = eval a b in 
						interpneg (c@(List.tl stackdfs)) answers

		;;




let rec interp stackdfs answers = 
		if (List.length stackdfs = 0 ) then answers else  
		let (a,b) = (List.nth stackdfs 0) in 
			if (List.length a)=0 then 
				let answers =[b]@answers in 
					interp (List.tl stackdfs) answers 
			else  
				let eval list_of_goals subst_list=
					let unify goal other_goals subst = 
						let Fact(a,b,c) =goal in  
							if Hashtbl.mem symb b = false then 
								(printf "Undefined Procedure\n";[])
							else 
								if a = 0 then 
									let rec fillst othergoals param factlist subst c index=
									  if index = (List.length factlist) then c else 
	  									try let t = List.nth factlist index in 
	  										match t with 
												Fact(x,y,z) -> let f = mgu (B("-1",param)) (B("-1",z)) (subst) in 
													let ogoals = (List.map (instrsubst f) othergoals) in 
									fillst othergoals param factlist subst ([(ogoals,f)]@c) (index+1)
			| Rule(x,y,z) -> 	
								let B(q,g)=y in 
								let f = instrsubst (varsub (qwerty [Rule(x,y,z)]) !x1) (Rule(x,y,z)) in x1:=!x1+(List.length (qwerty [Rule(x,y,z)] ));
								let Rule(i,B(jk,o),p) = f in let gh = matchterms (ui (Hashtbl.create 1)) (B(jk,o)) (B(jk,param)) in 
									fillst othergoals param factlist subst ([(((List.map (instrsubst gh) p)@othergoals),subst)]@c) (index+1)
			
	with  No_Match -> fillst othergoals param factlist subst (c) (index+1)
								in fillst other_goals c (Hashtbl.find prog b) subst [] 0
								else 
									let bv = interpneg [([Fact(0,b,c)],(ui (Hashtbl.create 1)))] [] in 
										if (List.length bv) <> 0 then [] 
										else [(other_goals,subst)] in 
					unify (List.nth list_of_goals 0) (List.tl list_of_goals) subst_list in 
					let c = eval a b in 
						interp (c@(List.tl stackdfs)) answers

		;;

let rec prlist t id  =
	if id <> List.length t then (
	let x  = List.nth t id in 
		 (match x with 
		Sym y-> printf "%s" y
		| Var y ->  printf "%s" y
		| B(z,y) -> printf "%s" z; prlist y 0);
		  if id = List.length t -1 then () else printf "," ;flush stdout;prlist t (id+1)) else (printf")";flush stdout);;

(*let custom = 
	let a = Hashtbl.find prog "father" in 
		for i = 0 to List.length a -1 do 
			let Rule(b,c,d)= List.nth a i in 
		let B(x,z) = b in 

(*let rec offset  hashtb index t = 
		match t with 
		| Sym a -> (Sym a, hashtb,index)
		| Var z -> if Hashtbl.mem hashtb (Var z) = true then 
						(Hashtbl.find hashtb (Var z) ,hashtb,index)
				   else let _ = Hashtbl.add hashtb (Var z) (Var (String.concat "" ["z";string_of_int(index)])) in 
				   		(Hashtbl.find hashtb (Var z) ,hashtb,(index+1) )
		| B(a,b) -> B(a,)

*)*)

let moreans = ref false;;
let queryvar = ref [];;

let printres answers index varlist = 
	if List.length varlist = 0 then if List.length answers <> 0 then (printf "true\n";printf "?-  ";moreans:=false;flush stdout) else (printf "false\n";printf "?- ";moreans:=false) else 
		if index = List.length answers then (printf "false\n";printf"?-  "; moreans:=false)
		else let subfun = List.nth answers index in 
			for i = 0 to (List.length varlist)-1 do
				let Var x = List.nth varlist i in 
					match (subfun (Var x)) with 
					Sym y -> printf "%s" x; printf ":-\t%s" y; printf "\n";flush stdout;
					| Var y -> printf"%s " x; printf ":-\t%s" y;printf "\n"; flush stdout;
					| B(y,z) -> printf "%s" x; printf ":-\t%s" y ;printf "(";prlist z 0 
			done ;;


# 276 "plgparser.ml"
let yytransl_const = [|
  259 (* RPAREN *);
  260 (* LPAREN *);
  261 (* COMMA *);
  262 (* STOP *);
  263 (* IF *);
  264 (* NOT *);
  265 (* SEMI *);
  266 (* NEWLINE *);
  267 (* END *);
    0|]

let yytransl_block = [|
  257 (* VARS *);
  258 (* PRED *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\005\000\005\000\003\000\003\000\006\000\006\000\006\000\007\000\
\007\000\007\000\007\000\007\000\007\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\000\000\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\002\000\002\000\001\000\001\000\002\000\
\001\000\002\000\001\000\002\000\002\000\005\000\006\000\001\000\
\001\000\004\000\006\000\003\000\003\000\002\000\003\000\003\000\
\004\000\005\000\006\000\006\000\007\000\002\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\007\000\000\000\006\000\
\009\000\031\000\003\000\000\000\000\000\011\000\002\000\000\000\
\000\000\000\000\022\000\000\000\008\000\005\000\010\000\004\000\
\000\000\013\000\012\000\000\000\000\000\000\000\024\000\000\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\000\000\021\000\000\000\020\000\000\000\026\000\000\000\
\014\000\000\000\000\000\028\000\000\000\027\000\015\000\000\000\
\029\000\019\000"

let yydgoto = "\003\000\
\004\000\010\000\015\000\011\000\012\000\016\000\030\000"

let yysindex = "\017\000\
\000\000\001\255\000\000\015\255\024\255\000\000\003\255\000\000\
\000\000\000\000\000\000\014\255\254\254\000\000\000\000\005\255\
\038\255\025\255\000\000\031\255\000\000\000\000\000\000\000\000\
\038\255\000\000\000\000\021\255\039\255\028\255\000\000\038\255\
\025\255\000\000\048\255\038\255\038\255\038\255\040\255\049\255\
\000\000\041\255\000\000\050\255\000\000\025\255\000\000\044\255\
\000\000\025\255\037\255\000\000\025\255\000\000\000\000\038\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\054\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\052\255\053\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\054\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\244\255\000\000\000\000\232\255"

let yytablesize = 57
let yytable = "\024\000\
\035\000\025\000\005\000\026\000\020\000\031\000\006\000\040\000\
\007\000\008\000\009\000\043\000\044\000\045\000\027\000\005\000\
\013\000\001\000\002\000\021\000\041\000\007\000\022\000\023\000\
\014\000\036\000\005\000\017\000\018\000\019\000\039\000\058\000\
\007\000\052\000\032\000\033\000\034\000\055\000\028\000\029\000\
\057\000\056\000\037\000\038\000\046\000\047\000\049\000\050\000\
\053\000\054\000\042\000\048\000\051\000\030\000\017\000\016\000\
\018\000"

let yycheck = "\012\000\
\025\000\004\001\002\001\006\001\002\001\018\000\006\001\032\000\
\008\001\009\001\010\001\036\000\037\000\038\000\010\001\002\001\
\002\001\001\000\002\000\006\001\033\000\008\001\009\001\010\001\
\010\001\005\001\002\001\004\001\005\001\006\001\003\001\056\000\
\008\001\046\000\004\001\005\001\006\001\050\000\001\001\002\001\
\053\000\005\001\004\001\005\001\005\001\006\001\006\001\007\001\
\005\001\006\001\003\001\003\001\003\001\000\000\003\001\003\001\
\003\001"

let yynames_const = "\
  RPAREN\000\
  LPAREN\000\
  COMMA\000\
  STOP\000\
  IF\000\
  NOT\000\
  SEMI\000\
  NEWLINE\000\
  END\000\
  "

let yynames_block = "\
  VARS\000\
  PRED\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 272 "plgparser.mly"
           ()
# 386 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 273 "plgparser.mly"
                      ()
# 394 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 276 "plgparser.mly"
           (
							queryvar:=(qwerty (_1));
							x1:=0;
							let query = [(_1,(ui (Hashtbl.create 1)))] in 
								queryans:= interp query []; 
								moreans:=true;printres (!queryans) 0 (!queryvar);try queryans:= (List.tl (!queryans)) with Failure "tl" -> moreans:=false;flush stdout)
# 406 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nlines) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 282 "plgparser.mly"
                    (
							queryvar:= (qwerty _2);
							x1:=0;
							let query = [(_2,(ui (Hashtbl.create 1)))] in 
								queryans:= interp query []; 
								moreans:=true;printres (!queryans) 0 (!queryvar);try queryans:= (List.tl (!queryans)) with Failure "tl" -> moreans:=false;flush stdout)
# 419 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nlines) in
    Obj.repr(
# 288 "plgparser.mly"
                   (if (!moreans)= true then ( (printres (!queryans) 0 (!queryvar)); if(!moreans) = true then queryans:=(List.tl (!queryans)) else moreans:=false) else (printf "ERROR\n";printf "?- ");flush stdout)
# 426 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 289 "plgparser.mly"
            (if (!moreans)= true then ( (printres (!queryans) 0 (!queryvar)); if(!moreans) = true then queryans:=(List.tl (!queryans)) else moreans:=false)else (printf "ERROR\n";printf "?- ");flush stdout;)
# 432 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 290 "plgparser.mly"
            (moreans:=false;printf "?- ";flush stdout)
# 438 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nlines) in
    Obj.repr(
# 291 "plgparser.mly"
                  (moreans:=false;printf "?- ";flush stdout)
# 445 "plgparser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 294 "plgparser.mly"
                   ()
# 451 "plgparser.ml"
               : 'nlines))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nlines) in
    Obj.repr(
# 295 "plgparser.mly"
                      ()
# 458 "plgparser.ml"
               : 'nlines))
; (fun __caml_parser_env ->
    Obj.repr(
# 298 "plgparser.mly"
                   ( )
# 464 "plgparser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 299 "plgparser.mly"
                   ( (*printf "%d\n" (!a); a:= !a+1 ; flush stdout*))
# 471 "plgparser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 303 "plgparser.mly"
               ( 
							if Hashtbl.mem symb _1 = true then 
								if Hashtbl.find symb _1 = 0 then 
									() 
								else raise Not_WellFormed 
							else 
								( Hashtbl.add symb _1 0 ; 
								  Hashtbl.add prog _1 [Fact(0,_1,[])] ) 
							  )
# 486 "plgparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'mixed) in
    Obj.repr(
# 312 "plgparser.mly"
                                     ( 
							if Hashtbl.mem symb _1 = true then 
								if Hashtbl.find symb _1 = (List.length _3) then 
									Hashtbl.replace prog _1 ((Hashtbl.find prog _1)@[Fact(0,_1,_3)]) 
								else raise Not_WellFormed 
							else ( Hashtbl.add symb _1 (List.length _3); 
								   Hashtbl.add prog _1 [Fact(0,_1,_3)] )
						                             )
# 501 "plgparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'mixed) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 320 "plgparser.mly"
                                        (
							if Hashtbl.mem symb _1 = true then 
								if Hashtbl.find symb _1 = (List.length _3) then 
									Hashtbl.replace prog _1 ((Hashtbl.find prog _1)@[Rule(0,B(_1,_3),_6)]) 
								else raise Not_WellFormed 
							else ( Hashtbl.add symb _1 (List.length _3); 
								   Hashtbl.add prog _1 [Rule(0,B(_1,_3),_6)] ) 
						)
# 517 "plgparser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 329 "plgparser.mly"
                ( [Sym _1] )
# 524 "plgparser.ml"
               : 'mixed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 330 "plgparser.mly"
            ( [Var _1] )
# 531 "plgparser.ml"
               : 'mixed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'mixed) in
    Obj.repr(
# 331 "plgparser.mly"
                                ([B(_1,_3)])
# 539 "plgparser.ml"
               : 'mixed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'mixed) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'mixed) in
    Obj.repr(
# 332 "plgparser.mly"
                                            (B(_1,_3)::_6 )
# 548 "plgparser.ml"
               : 'mixed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mixed) in
    Obj.repr(
# 333 "plgparser.mly"
                        ( (Sym _1)::_3)
# 556 "plgparser.ml"
               : 'mixed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mixed) in
    Obj.repr(
# 334 "plgparser.mly"
                        ( (Var _1)::_3)
# 564 "plgparser.ml"
               : 'mixed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 336 "plgparser.mly"
                     ( [Fact(0,_1,[])])
# 571 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 337 "plgparser.mly"
                     ( [Fact(1,_2,[])])
# 578 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 338 "plgparser.mly"
                       (Fact(0,_1,[])::_3)
# 586 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 339 "plgparser.mly"
                           (Fact(1,_2,[])::_4)
# 594 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'mixed) in
    Obj.repr(
# 340 "plgparser.mly"
                                     ( [Fact(0,_1,_3)])
# 602 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'mixed) in
    Obj.repr(
# 341 "plgparser.mly"
                                         ([Fact(1,_2,_4)] )
# 610 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'mixed) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 342 "plgparser.mly"
                                           ( Fact(0,_1,_3)::_6 )
# 619 "plgparser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'mixed) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 343 "plgparser.mly"
                                               (Fact(1,_2,_4)::_7)
# 628 "plgparser.ml"
               : 'body))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry interactive *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
let interactive (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : unit)
;;
# 346 "plgparser.mly"

# 659 "plgparser.ml"
