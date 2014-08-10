let sigmasig = Hashtbl.create 10;;

exception Negative_arity;;
exception Symbol_repeated;;

let rec validSig n =
	match n with 
		[] -> true
		| x::xs -> 
			let (a,b) = x in if b<0 then raise Negative_arity else if Hashtbl.mem sigmasig a = true then raise Symbol_repeated 
		else Hashtbl.add sigmasig a b;
		validSig xs

	;;

type preterm = A of string | B of string*(preterm list) | Var of string;;

let rec wf s = 
	match s with 
	(A x) ->	if(Hashtbl.mem sigmasig x)= true then (if Hashtbl.find sigmasig x =0 then true else false ) else false
	| (Var x) -> true
	| B (x,y) -> 
			begin 
				if (Hashtbl.mem sigmasig x)= true then (if Hashtbl.find sigmasig x = List.length y then 
					(
						let a = ref true in for i =1 to List.length y do 
							a :=  ((!a) & (wf (List.nth y (i-1)))) done ;
							(!a)


					)

					 else false) else false;
			end
		;;

let substtab = Hashtbl.create 10;;
let bn a b = a || b;;
let rec equate a b = match b with 
		 (A x) -> false
		| (Var x) -> if b = a then true else false
		| (B(x ,y)) -> 	List.fold_left bn false (List.map (equate a) y)
	;;
let rec ui  a = function (A x) -> (A x)
				| (Var y) -> if Hashtbl.mem a (Var y) = true then Hashtbl.find a (Var y) else (Var y)
				| B(x,y) -> B(x,(List.map (ui a) y));;
(*
let rec compose f g = function (A x) -> (A x)
					| (Var x) -> g(f(Var x))
					| B(x,y) -> B(x,(List.map (compose f g) y));;*)
					
let compose f g = function x -> g(f(x));;
let rec subst f t = match t with 
		(A x) -> 	 (A x)
		| (Var p) ->  f (Var p)
		| B(x,y)  -> B(x,List.map (subst f) y)
		;;   

(*let rec subst t s = 
	match s with 
	 (A x) -> (A x)
	 | (Var p) -> if (Hashtbl.mem t (Var p)) = true then (Hashtbl.find t (Var p)) else (Var p)
	 | B(x,y) -> B(x, List.map (subst t) y )
	;;*)

exception NotUnif;;
let rec mgu f t1 t2  = let t3= subst f t1 in let t4 = subst f t2 in match (t3,t4) with 
			(A x,A y) -> if (x=y) then f else raise NotUnif
			| (A x,B(y,z)) -> raise NotUnif
			| (A x,Var y) -> let z = Hashtbl.create 1 in let ()=Hashtbl.add z (Var y) (A x) in compose f (ui z)
			| (Var y,A x) -> let z = Hashtbl.create 1 in let () = Hashtbl.add z (Var y) (A x) in compose f (ui z)
			| (Var x,Var y) -> if x=y then f else let z = Hashtbl.create 1 in let () = (Hashtbl.add z (Var x) (Var y)) in compose f (ui z)
			| (Var x,B(y,z)) ->  if ((equate (Var x) (B(y,z))) = false) then let p = Hashtbl.create 1 in let () = Hashtbl.add p (Var x) (B(y,z)) in compose f (ui p) else raise NotUnif
			| (B(x,y),B(w,z)) ->if x=w then 
					let rec dp p a b = match (a,b) with 
						([],[]) -> p
						| (x::xs,y::ys) -> let g = mgu p x y in dp g xs ys 
						in 
						let n = dp f y z in n else raise NotUnif 
			| (B(x,y),(A s)) -> raise NotUnif
			| (B(x,y),(Var z)) ->  if (equate (Var z) (B(x,y))) = false then let p = Hashtbl.create 1 in let () = Hashtbl.add p (Var z) (B(x,y)) in compose f (ui p) else raise NotUnif
	;;

let rec setmgu f b t = match t with
			[] -> f
			| x::xs -> let c = mgu f b x in setmgu c x xs
			;;

let callsetmgu t = setmgu (ui (Hashtbl.create 1)) (List.nth t 0) (List.tl t);; 




