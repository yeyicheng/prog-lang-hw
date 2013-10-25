(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* problem 1 *)
fun only_capitals(strs: string list)=
	List.filter (fn s => Char.isUpper(String.sub(s, 0))) strs

(* problem 2 *)
fun longest_string1(strs: string list)=
	List.foldl (fn (x, y)=> if String.size x > String.size y then x else y) "" strs
	
(* problem 3 *)
fun longest_string2(strs: string list)=
	List.foldl (fn (x, y)=> if String.size x >= String.size y then x else y) "" strs
	
(* problem 4 *)
fun longest_string_helper func strs =
	List.foldl (fn(x, y) => if func(String.size x, String.size y) then x else y) "" strs

(* problem 4a *)
fun longest_string3(strs: string list)=
	let val helper = longest_string_helper (fn(x, y) => x > y)
	in
		helper strs
	end

(* problem 4b *)
fun longest_string4(strs: string list)=
	let val helper = longest_string_helper (fn(x, y) => x >= y)
	in
		helper strs
	end
	
(* problem 5 *)
fun longest_capitalized (strs: string list)=
	let val helper = (longest_string1 o only_capitals)
	in	helper strs
	end

(* problem 6*)
fun rev_string (str: string)=
	(implode o rev o explode) str
	
(* problem 7 *)
fun first_answer func lst=
	case lst of 
		  [] => raise NoAnswer
		| x::xs => case func x of
						  NONE => first_answer func xs
						| SOME y => y
	
(* problem 8 *)
fun all_answers func lst=
	let fun helper (f, l, acc)=
		case l of
			  [] => SOME acc
			| x::xs => case f x of
							  NONE => NONE
							| SOME y => helper (f, xs, acc@y)
	in 
		helper(func, lst, [])
	end

(* problem 9a *)
fun count_wildcards p=
	g (fn _ => 1) (fn x => 0) p
	
(* problem 9b *)
fun count_wild_and_variable_lengths p=
	g (fn _ => 1) String.size p

(* problem 9c *)	
fun count_some_var (s: string, p: pattern) =
  g (fn _ => 0) (fn x => if x = s then 1 else 0) p
  
 (* problem 10 *)
fun check_pat(p) = 
    let fun list_vars (Variable x) = [x]
			| list_vars (TupleP ps) = List.foldl (fn (p', acc) => acc @list_vars(p')) [] ps
			| list_vars (ConstructorP(_, p')) = list_vars p'
			| list_vars (_) = []
	fun has_repeats ([]) = false
		| has_repeats (x::xs) = List.exists (fn x' => x = x') xs orelse has_repeats xs
    in
		(not o has_repeats o list_vars) p
    end

(* problem 11 *)
fun match(v, p) = 
    case (p, v) of
		(Wildcard, _) => SOME []
      | (Variable s, _) => SOME [(s,v)]
      | (UnitP, Unit) => SOME []
      | (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs 
				 then all_answers (fn (vs',ps') => match(vs',ps')) (ListPair.zip(vs,ps))
				 else NONE
      | (ConstructorP(s1,pp), Constructor(s2,pv)) => if s1 = s2 then match(pv,pp) else NONE
      | _ => NONE
 
 (* problem 12 *)
fun first_match v ps = 
    ( SOME(first_answer (fn p => match(v,p)) ps) ) handle NoAnswer => NONE