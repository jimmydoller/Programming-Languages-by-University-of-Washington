(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(* 1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals(strs : string list) = 
    let
      fun is_starts_with_cap(str : string) = Char.isUpper(String.sub(str, 0))
    in
        List.filter (is_starts_with_cap) strs
    end
        
(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)
fun longest_string1(strs : string list)
    = List.foldl(fn (str, acc) => if String.size(str) > String.size(acc) then str else acc) "" strs

(* 3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size. *)
fun longest_string2(strs : string list)
    = List.foldl(fn (str, acc) => if String.size(str) >= String.size(acc) then str else acc) "" strs

(* 4. Write functions longest_string_helper, longest_string3, and longest_string4 such that: *)
fun longest_string_helper comparator strs =
            List.foldl (fn (str, acc) => if comparator(String.size(str), String.size(acc)) then str else acc) "" strs

val longest_string3 = longest_string_helper (fn (x1, x2) => x1 > x2) 
val longest_string4 = longest_string_helper (fn (x1, x2) => x1 >= x2)

(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. *)
fun longest_capitalized(strs : string list) = 
    let
        val get_longest_capitalized = longest_string3 o only_capitals
    in
        get_longest_capitalized strs
    end

(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. *)
fun rev_string(str : string) =
    let 
        val get_rev_string = String.implode o List.rev o String.explode
    in
        get_rev_string str
    end

(* 7. Write a function first_answer of type ('a -> 'b option) -> 'a list -> 'b (notice the 2 argu-
ments are curried). The rst argument should be applied to elements of the second argument in order
until the rst time it returns SOME v for some v and then v is the result of the call to first_answer.
If the rst argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy. *)
fun first_answer func li = case li of
        [] => raise NoAnswer
        | x::xs' => case func(x) of
            NONE => first_answer func xs'
            | SOME(v) => v

(* 8. Write a function all_answers of type ('a -> 'b list option) -> 'a list -> 'b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the rst argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn't matter).  *)
fun all_answers func li = case li of
    [] => SOME([])
    | x::xs' => case func(x) of
        NONE => NONE
        | SOME(ylist) => case all_answers func xs' of
            NONE => NONE
            | SOME(zlist) => SOME(ylist@zlist)

(* Pattern problems *)
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

(* 9a. Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains. *)
fun count_wildcards(pat : pattern) = case pat of
    Wildcard => 1
    | ConstructorP(str, constructorPat) => count_wildcards(constructorPat)
    | TupleP(tupPats) => (case tupPats of
        [] => 0
        | tupPat::tupPats' => count_wildcards(tupPat) + count_wildcards(TupleP(tupPats')))
    | _ => 0

(* 9b. Use g to dene a function count_wild_and_variable_lengths that takes a pattern and returns
the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. *)
fun count_wild_and_variable_lengths(pat : pattern) = 
    let
        fun sum_variable_lengths(pat2 : pattern) = case pat2 of
            Variable(x) => String.size(x)
            | ConstructorP(str, constructorPat) => sum_variable_lengths(constructorPat)
            | TupleP(tupPats) => (case tupPats of
                [] => 0
                | tupPat::tupPats' => sum_variable_lengths(tupPat) + sum_variable_lengths(TupleP(tupPats')))
            | _ => 0
    in
        count_wildcards(pat) + sum_variable_lengths(pat)
    end

(* 9c. Use g to dene a function count_some_var that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern. We care only about
variable names; the constructor names are not relevant. *)
fun count_some_var(str : string, pat : pattern) = case pat of
    Variable(x) => if x = str then 1 else 0
    | ConstructorP(str, constructorPat) => count_some_var(str, constructorPat)
    | TupleP(tupPats) => (case tupPats of
        [] => 0
        | tupPat::tupPats' => count_some_var(str, tupPat) + count_some_var(str, TupleP(tupPats')))
    | _ => 0

(* 10. Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use dierent strings). The constructor
names are not relevant. *)
fun check_pat(pat : pattern) = 
    let
        fun get_strings(pat2 : pattern, strs : string list) = case pat2 of
            Variable(x) => x::strs
            | ConstructorP(str, constructorPat) => get_strings(constructorPat, strs)
            | TupleP(tupPats) => (case tupPats of
                [] => strs
                | tupPat::tupPats' => get_strings(tupPat, strs) @ get_strings(TupleP(tupPats'), strs))
            | _ => strs
        fun duplicates(strs2 : string list) = case strs2 of
            [] => false
            | str2::strs2' => if List.exists (fn (x) => x = str2) strs2' then true else duplicates(strs2')
    in
        not (duplicates(get_strings(pat, [])))
    end

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* 11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. *)
fun match(valu : valu, pat : pattern) = case pat of
    Wildcard => SOME []
	| Variable(s) => SOME [(s, valu)]
    | UnitP => if valu = Unit then SOME [] else NONE
    | ConstP(x) => if valu = Const(x) then SOME [] else NONE
    | TupleP(pats) => (case valu of 
        Tuple(vals) => (all_answers match (ListPair.zipEq(vals, pats)) handle UnequalLengths => NONE)
        | _ => NONE)
    | ConstructorP(str1, pat1) => (case valu of
        Constructor(str2, val2) => if str1 = str2 then match(val2, pat1) else NONE
        | _ => NONE)
    (* | _ => NONE *)

(* 12. Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the rst pattern in the list that matches. Use first_answer and a
handle-expression. *)
fun first_match valu pats = 
    SOME (first_answer (fn (pat) => match(valu, pat)) pats) handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)