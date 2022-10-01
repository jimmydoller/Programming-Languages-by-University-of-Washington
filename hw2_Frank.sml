(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)




fun all_except_option (Str : string, Strs : string list)=
    let fun aux (Str : string, Strs: string list, acc: string list)=
	    case Strs of [] => NONE
		       | x::xs' => if same_string(Str,x) then SOME (acc@xs') else aux (Str, xs', acc@[x])
    in
	aux(Str, Strs, [])
    end

fun get_substitutions1 (Subst: string list list, S: string) =
    case Subst of
	     [] => []
          |  x::[] => ( case all_except_option (S,x) of NONE => []
						| SOME y => y)
	  |  x::xs' => case all_except_option (S,x) of NONE => get_substitutions1 (xs',S)
						     | SOME y => y@get_substitutions1(xs',S)


fun get_substitutions2 (Subst: string list list, S: string) =
    let fun tail (Subst: string list list, S: string, concat: string list)=
	case Subst of
            [] => []
	 |  x::[] => ( case all_except_option (S,x) of NONE => concat
						     | SOME y => concat@y)
	 |  x::xs' => case all_except_option (S,x) of NONE => tail (xs',S, concat)
						    | SOME y => tail (xs',S, concat@y)
    in
	tail(Subst, S, [])
    end

type full_name = {first:string,middle:string,last:string};

fun get_first_name (x:full_name)=
    case x of
	{first, middle, last} => first

fun get_middle_name (x:full_name)=
    case x of
	{first, middle, last} => middle

fun get_last_name (x:full_name)=
    case x of
	{first, middle, last} => last

fun similar_names (Subst: string list list, Fullname: {first:string,middle:string,last:string}) =
    let
	val firstname=get_first_name(Fullname)
	val middlename =get_middle_name(Fullname)
	val lastname=get_last_name(Fullname)
	val allsubs = get_substitutions2(Subst, firstname)
	fun acc (subs: string list, acum: {first:string,middle:string,last:string} list)=
	    case subs of [] => acum
		       | x::xs' => acc(xs', acum@[{first=x ,middle=middlename, last=lastname}])
    in
	acc (allsubs, [Fullname])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove


fun card_color (inputcard : card)=
    case inputcard of (Clubs,_) => Black
		   | (Diamonds,_) => Red
		   | (Hearts,_) => Red
		   | (Spades,_) => Black

fun card_value (inputcard: card)=
    case inputcard of (_, Jack) => 10
		    | (_, Queen) => 10
		    | (_, King) => 10
		    |  (_, Ace) => 11
		    |  (_, Num (i))  => i

fun remove_card (cards: card list, toremove : card, e ) =  (*PUT THE EXCEPTION IN*)
    let
	val acc=[]
	fun appender (cards: card list, toremove : card, acc)=
	    case cards of [] => acc
			| x::xs'  => if x = toremove then appender(xs', toremove, acc) else appender(xs', toremove, acc@[x])
    in
	if appender(cards, toremove, acc) =cards then raise e  else appender(cards, toremove, acc)

(*if appender(cards, toremove, acc)<>cards then acc else raise e*)
    end

fun all_same_color (cards: card list)=
    case cards of
	[] =>true
     | _::[] => true
     | head::(neck::rest) => card_color(head)=card_color(neck) andalso all_same_color (neck::rest)

fun sum_cards (cards: card list)=
    let
	fun tail (cards: card list, sum: int)=
	    case cards of []=>sum
			| x::xs' => tail(xs', sum + card_value(x))
    in
	tail(cards,0)
    end


fun score (cards: card list, goal: int)=
    let
	val sum=sum_cards(cards)
	fun finalize_score ( prelim_score: int, cards: card list)=
	    if all_same_color(cards)=true then (prelim_score div 2) else prelim_score
    in
	if sum>goal then finalize_score((3*(sum-goal)), cards) else finalize_score((goal-sum), cards)
    end


fun officiate (cards: card list, moves: move list, goal: int)=
    let
	val held =[]
	fun move (cards: card list, moves: move list, goal: int, held:card list)=
	    case moves of [] => held
			| x::xs' => (case x of
					Discard (card) => move (cards, xs', goal, remove_card (held, card, IllegalMove))
				        | Draw  => (case cards of
						     [] => held
						     | b::bs' => if (sum_cards (held@[b]) > goal) then held@[b] else move (bs', xs', goal, held@[b]) ))
    in
	score(move(cards, moves , goal, held), goal)
    end






(* put your solutions for problem 2 here *)



(*all_same_color([(Clubs, Jack), (Clubs, Ace), (Spades, King)])*)
