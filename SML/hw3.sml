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

fun only_capitals str_lst =
  List.filter (fn x => Char.isUpper (String.sub (x, 0))) str_lst

fun longest_string1 str_lst =
  foldl (fn (x, y) => if String.size x > String.size y then x else y) "" str_lst

fun longest_string2 str_lst =
  foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" str_lst

fun longest_string_helper comparer str_lst =
  foldl (fn (x, y) => if comparer (String.size x, String.size y) then x else y) "" str_lst

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer find_option lst =
  case lst of
       [] => raise NoAnswer
     | hd::tl => case find_option hd of
                      SOME x => x
                    | NONE => first_answer find_option tl

fun all_answers find_option lst =
  let
    fun accumulate (l, acc) =
      case l of
           [] => SOME acc
         | hd::tl => case find_option hd of
                          NONE => NONE
                        | SOME x => accumulate (tl, (x @ acc))
  in
    accumulate (lst, [])
  end

fun count_wildcards p =
  g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
  g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (str, p) =
  g (fn _ => 0) (fn x => if x = str then 1 else 0) p

fun check_pat p =
  let
    fun get_lst p =
      case p of
           Variable x => [x]
         | TupleP ps => foldl (fn (p, acc) => (get_lst p) @ acc) [] ps
         | ConstructorP (_, p) => get_lst p
         | _ => []
    fun check_dupes str_lst =
      case str_lst of
           [] => true
         | hd::tl => if List.exists (fn x => hd = x) tl
                     then false
                     else check_dupes tl
  in
    (check_dupes o get_lst) p
  end

fun match (v, p) =
  case (v, p) of
       (_, Wildcard) => SOME []
     | (_, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const c, ConstP const) => if c = const then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                then all_answers match (ListPair.zip (vs, ps))
                                else NONE
     | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2
                                                      then match (v, p)
                                                      else NONE
     | _ => NONE


fun first_match v ps =
  let
    fun make_pairs (old_ps, new_ps) =
      case old_ps of
           [] => new_ps
         | hd::tl => make_pairs (tl, (v, hd)::new_ps)
    val pairs = make_pairs (ps, [])
  in
    SOME (first_answer match pairs)
  end
  handle NoAnswer => NONE
