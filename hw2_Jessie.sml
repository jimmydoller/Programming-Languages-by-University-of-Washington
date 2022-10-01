(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
	     
(* a *)
fun all_except_option (str, str_list) =	    
    case str_list of
	[] => NONE
     |  x::rest => if same_string(str, x)
		   then SOME rest
		   else case all_except_option(str, rest) of
			       NONE => NONE
			     | SOME xs => SOME (x::xs)
           
(* b *)
fun get_substitutions1 (substitutions, s) =
    case substitutions of
	[] => []
      | curr_list::rest =>  case all_except_option(s, curr_list) of
				NONE => get_substitutions1(rest, s)
			      | SOME lst => lst @ get_substitutions1(rest, s)

(* c *)								    
fun get_substitutions2 (substitutions, s) =
    let fun get_substitutions2_helper(lists, res) =
	    case lists of
		[] => res
	      | curr_list::rest_lists => case all_except_option(s, curr_list) of
				NONE => get_substitutions2_helper(rest_lists, res)
			      | SOME lst => get_substitutions2_helper(rest_lists, res @ lst)
    in
	get_substitutions2_helper(substitutions, [])
    end
	    
(* d *)
fun similar_names (substitutions, {first=f ,middle=m, last=l}) =
    let val subs = get_substitutions2(substitutions, f)
	fun similar_names_helper(list) =
	    case list of
		[] => []
	      | x::xs => {first=x, middle=m, last=l}::similar_names_helper(xs)
    in
	 {first=f, middle=m, last=l}::similar_names_helper(subs)
    end

	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
	      
(* a *)
fun card_color c =
    case c of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | _ => Red
		 
(* b *)
fun card_value c =
    case c of
	(_, Num x) => x
      | (_, Ace) => 11
      | _ => 10
		 
(* c *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e (* If c is not in the list, raise the exception e *)
      | x::xs => if x = c
		 then xs 
		 else x::remove_card(xs, c, e)

(* d *)
fun all_same_color cs =
    case cs of
	[] => true
      | _::[] => true
      | c1::(c2::rest) => card_color(c1) = card_color(c2) andalso all_same_color(c2::rest)

(* e *)
fun sum_cards cs =
    let fun sum_cards_helper(list, sum) =
	    case list of
		[] => sum
	      | x::xs => sum_cards_helper(xs, card_value(x) + sum)
    in
	sum_cards_helper(cs, 0)
    end
	
(* f *)
fun preliminary_score (sum, goal) =
    if sum > goal then 3 * (sum - goal) else (goal - sum)
						 
fun score (held_cards, goal) =
    let val pre_score = preliminary_score(sum_cards(held_cards), goal)
    in
	if all_same_color(held_cards)
	then pre_score div 2
	else pre_score
    end
	
(* g *)
fun officiate (card_list, move_list, goal) =
    let fun officiate_helper (curr_move_list, curr_card_list, held_cards) =
	    case (curr_move_list, curr_card_list) of
		([], _) => score(held_cards, goal) (* The game ends if there are no more moves. *)
	      | (Discard c::rest_moves, _) =>  (* If the player discards some card c, play continues *)
		officiate_helper(rest_moves, curr_card_list, remove_card(held_cards, c, IllegalMove) )
	      | (Draw::rest_moves, []) => score(held_cards, goal) (* Attempts to draw but the card-list is empty. *)
	      | (Draw::rest_moves, first_card::rest_cards) =>
		let val new_held = first_card::held_cards
		in
		    if sum_cards(new_held) > goal
		    then score(new_held, goal)
		    else officiate_helper(rest_moves, rest_cards, new_held)	      
		end					   
    in
	officiate_helper(move_list, card_list, [])
    end
	


(* problem 3 *)
(* a *)
fun sum_cards_and_count_ace cs =
    let fun is_ace c =
	    case c of
		(_, Ace) => 1
	      | _ => 0 
			 
	fun sum_cards_helper (list, sum, ace_count) =
	    case list of
		[] => (sum, ace_count)
	      | x::xs => sum_cards_helper(xs, card_value(x) + sum, is_ace(x) + ace_count)
    in
	sum_cards_helper(cs, 0, 0)
    end

fun score_challenge (held_cards, goal) =
    let	fun best_pre_score (sum, goal, ace_count) =
	    if ace_count = 0 orelse sum - goal < 3
	    then preliminary_score(sum, goal)
	    else best_pre_score(sum - 10, goal, ace_count - 1)
			       
	val (sum, ace_count) = sum_cards_and_count_ace(held_cards)
	val pre_score = best_pre_score(sum, goal, ace_count)
    in
	if all_same_color(held_cards)
	then pre_score div 2
	else pre_score
    end	     

fun officiate_challenge (card_list, move_list, goal) =
    let fun officiate_challenge_helper (curr_move_list, curr_card_list, held_cards) =
	    case (curr_move_list, curr_card_list) of
		([], _) => score_challenge(held_cards, goal) 
	      | (Discard c::rest_moves, _) => 
		officiate_challenge_helper(rest_moves, curr_card_list, remove_card(held_cards, c, IllegalMove) )
	      | (Draw::rest_moves, []) =>    
		score_challenge(held_cards, goal)
	      | (Draw::rest_moves, first_card::rest_cards) =>
		let val new_held = first_card::held_cards
		    val (sum, ace_count) = sum_cards_and_count_ace(new_held)
		    val least_sum = sum - ace_count * 10
		in
		    if least_sum > goal
		    then score_challenge(new_held, goal)
		    else officiate_challenge_helper(rest_moves, rest_cards, new_held)	      
		end					   
    in
	officiate_challenge_helper(move_list, card_list, [])
    end

(* b *)
fun careful_player (card_list, goal) =
    let fun careful_player_helper(card_list, held_cards) =
	    let val curr_sum = sum_cards(held_cards)
	    in
		if curr_sum = goal                 (* score will be 0 if sum = goal *)
		then []                            (* no more moves *)
		else if goal - curr_sum > 10       (* must draw if goal is >10 greater than sum *)
		then case card_list of
			 [] => [Draw]
		       | x::xs =>  Draw::careful_player_helper(xs, x::held_cards)
		else
		    case card_list of
			[] => []
		      | x::xs =>
			let fun find_card_with_value (cs,v) =
				case cs of
				    [] => NONE
				  | x::xs => if card_value(x) = v
					     then SOME x
					     else find_card_with_value(xs, v)
			    
			    val next_card_value = card_value(x)
			in
			    case find_card_with_value(held_cards, curr_sum + next_card_value - goal) of
				NONE => if curr_sum + next_card_value > goal
					then []
					else Draw::careful_player_helper(xs, x::held_cards)
			      | SOME c =>  Discard c::[Draw] (* if score is 0 after discarding c and drawing, then must done*)
			end
			    
	    end
    in
	careful_player_helper(card_list, [])
    end
