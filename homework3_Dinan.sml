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

fun only_capitals ss =
    List.filter (fn s => Char.isUpper(String.sub(s,0))) ss

fun longest_string1 ss =
    List.foldl 
	(fn (s, s_old) => if (String.size(s) > String.size(s_old))
			  then s else s_old)
	""
	ss

fun longest_string2 ss =
    List.foldl 
	(fn (s, s_old) => if (String.size(s) >= String.size(s_old))
			  then s else s_old)
	""
	ss

fun longest_string_helper f ss =
    List.foldl (fn (x, y) => if f(String.size(x), String.size(y))
			     then x else y)
	       ""
	       ss
	       
val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string  = String.implode o List.rev o String.explode 

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs' 

fun all_answers f lst =
    let
	fun helper xs acc = 
	    case (xs, acc) of
		([], _) => acc
	      | (x::xs', SOME v) =>
		case f x of
		    NONE => NONE
		  | SOME xv => helper xs' (SOME (xv @ v))
    in
	helper lst (SOME [])
    end

val count_wildcards = g (fn () => 1) (fn s => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size(s))

fun count_some_var (s,p) =
    g (fn () => 0) (fn x => if (s = x) then 1 else 0) p 

fun check_pat p =
    let
	fun helper1 (p,acc) =
	    case p of
		Variable x => [x]@acc
	      | TupleP ps => List.foldl helper1 acc ps
	      | ConstructorP(_,p) => helper1 (p, acc)
	      | _ => acc
	fun helper2 ss =
	    case ss of
		[] => true
	      | s::ss' => not (List.exists (fn x => x = s) ss') andalso helper2 ss'
    in
	helper2(helper1 (p, []))
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (x, Variable s) => SOME [(s, x)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if (x=y) then SOME [] else NONE
      | (Tuple vs, TupleP ps) =>
	if (List.length vs = List.length ps) then
	    all_answers match (ListPair.zip(vs, ps))
	else NONE
      | (Constructor(s2,v), ConstructorP(s1,p)) =>
	if (s1 = s2) then match (v, p) else NONE
      | _ => NONE 
				      
fun first_match v ps =
    SOME(first_answer (fn p => match(v,p)) ps)
		 handle NoAnswer => NONE
