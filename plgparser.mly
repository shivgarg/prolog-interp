%{
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


%}

%token <string> VARS
%token <string> PRED
%token RPAREN LPAREN COMMA STOP IF NOT SEMI
%token NEWLINE END
%start input
%start interactive
%type <unit> input
%type <unit> interactive

%%
input : 			{}
 	   				| input line {}
    ;
interactive :				
					body  {
							queryvar:=(qwerty ($1));
							x1:=0;
							let query = [($1,(ui (Hashtbl.create 1)))] in 
								queryans:= interp query []; 
								moreans:=true;printres (!queryans) 0 (!queryvar);try queryans:= (List.tl (!queryans)) with Failure "tl" -> moreans:=false;flush stdout}
					| nlines body  {
							queryvar:= (qwerty $2);
							x1:=0;
							let query = [($2,(ui (Hashtbl.create 1)))] in 
								queryans:= interp query []; 
								moreans:=true;printres (!queryans) 0 (!queryvar);try queryans:= (List.tl (!queryans)) with Failure "tl" -> moreans:=false;flush stdout}
					| nlines SEMI {if (!moreans)= true then ( (printres (!queryans) 0 (!queryvar)); if(!moreans) = true then queryans:=(List.tl (!queryans)) else moreans:=false) else (printf "ERROR\n";printf "?- ");flush stdout}
					| SEMI {if (!moreans)= true then ( (printres (!queryans) 0 (!queryvar)); if(!moreans) = true then queryans:=(List.tl (!queryans)) else moreans:=false)else (printf "ERROR\n";printf "?- ");flush stdout;}
					| STOP {moreans:=false;printf "?- ";flush stdout}
					| nlines STOP{moreans:=false;printf "?- ";flush stdout}


nlines	:			NEWLINE {}
					| nlines NEWLINE {}			


line : 				NEWLINE { }
					| exp NEWLINE { (*printf "%d\n" (!a); a:= !a+1 ; flush stdout*)}
					

exp:	
					PRED STOP { 
							if Hashtbl.mem symb $1 = true then 
								if Hashtbl.find symb $1 = 0 then 
									() 
								else raise Not_WellFormed 
							else 
								( Hashtbl.add symb $1 0 ; 
								  Hashtbl.add prog $1 [Fact(0,$1,[])] ) 
							  }
					| PRED LPAREN mixed RPAREN STOP { 
							if Hashtbl.mem symb $1 = true then 
								if Hashtbl.find symb $1 = (List.length $3) then 
									Hashtbl.replace prog $1 ((Hashtbl.find prog $1)@[Fact(0,$1,$3)]) 
								else raise Not_WellFormed 
							else ( Hashtbl.add symb $1 (List.length $3); 
								   Hashtbl.add prog $1 [Fact(0,$1,$3)] )
						                             }
					| PRED LPAREN mixed RPAREN IF body {
							if Hashtbl.mem symb $1 = true then 
								if Hashtbl.find symb $1 = (List.length $3) then 
									Hashtbl.replace prog $1 ((Hashtbl.find prog $1)@[Rule(0,B($1,$3),$6)]) 
								else raise Not_WellFormed 
							else ( Hashtbl.add symb $1 (List.length $3); 
								   Hashtbl.add prog $1 [Rule(0,B($1,$3),$6)] ) 
						} ;

mixed :				PRED { [Sym $1] }
					| VARS { [Var $1] }
					| PRED LPAREN mixed RPAREN {[B($1,$3)]}
					| PRED LPAREN mixed RPAREN COMMA mixed {B($1,$3)::$6 }
					| PRED COMMA mixed { (Sym $1)::$3}
					| VARS COMMA mixed { (Var $1)::$3};
			
body : 				PRED STOP { [Fact(0,$1,[])]}
					| NOT PRED STOP { [Fact(1,$2,[])]}
					| PRED COMMA body {Fact(0,$1,[])::$3}
					| NOT PRED COMMA body {Fact(1,$2,[])::$4}
					| PRED LPAREN mixed RPAREN STOP { [Fact(0,$1,$3)]}
					| NOT PRED LPAREN mixed RPAREN STOP {[Fact(1,$2,$4)] }
					| PRED LPAREN mixed RPAREN COMMA body { Fact(0,$1,$3)::$6 }
					| NOT PRED LPAREN mixed RPAREN COMMA body {Fact(1,$2,$4)::$7} ;

		%%

