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

(* Helpers *)
infix !>
fun x !> f = f(x)
			       
(* Problem 1 *)
fun only_capitals(texts) =
    List.filter (fn (x) => String.sub(x, 0) !> Char.isUpper) texts

(* Problem 2 *)
fun longest_string1(texts) =
    List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" texts

(* Problem 3 *)
fun longest_string2(texts) =
    List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" texts

(* Problem 4 *)
fun longest_string_helper f texts =
    List.foldl f "" texts

val longest_string3 =
    longest_string_helper (fn (x, y) => if String.size(x) > String.size(y) then x else y)

val longest_string4 = 
    longest_string_helper (fn (x, y) => if String.size(x) >= String.size(y) then x else y)

(* Problem 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* Problem 6 *)
val rev_string = String.implode o List.rev o String.explode

(* Problem 7 *)
fun first_answer f xs =
    case xs !> (List.map f) !> (List.filter isSome) of
	SOME x'::xs' => x'
      | _ => raise NoAnswer
		   
(* Problem 8 *)
fun all_answers f xs =
    let
	fun fold_helper(x1, x2) = if isSome x1 andalso isSome x2
				  then SOME ((valOf x2)@(valOf x1))
				  else NONE
    in
	xs !> (List.map f) !> (List.foldl fold_helper (SOME []))
    end

(* Problem 9 *)
fun count_wildcards p =
    g (fn x1 => 1) (fn y1 => 0) p

fun count_wild_and_variable_lengths p =
    g (fn x1 => 1) (fn y1 => String.size(y1)) p

fun count_some_var(s, p) =
    g (fn x1 => 0) (fn y1 => if y1 = s then 1 else 0) p

(* Problem 10 *)
fun check_pat p =
    let
	fun traverse_pattern p' =
	    case p' of
		Variable s => [s]
	      | TupleP ps => List.foldl (fn (p'', ss) => traverse_pattern(p'')@ss) [] ps
	      | ConstructorP(_, p'') => traverse_pattern(p'')
	      | _ => []
	fun has_all_unique_values xs =
	    case xs of
		[] => true
	      | x::xs' => if (List.exists (fn x' => x = x') xs')
			  then false
			  else has_all_unique_values(xs')
    in
	p !> traverse_pattern !> has_all_unique_values
    end

(* Problem 11 *)
fun match (v, p) =
    case (v, p) of
	(v, Wildcard) => SOME []
      | (v, Variable x) => SOME [(x, v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps)
				 then (vs, ps) !> ListPair.zip !> (all_answers match)
				 else NONE
      | (Constructor(s1,v'), ConstructorP(s2,p')) => if s1 = s2 then match (v', p') else NONE
      | _ => NONE

(* Problem 12 *)
fun first_match v ps =
    SOME (first_answer (fn x => match(v, x)) ps)
    handle NoAnswer => NONE
    
