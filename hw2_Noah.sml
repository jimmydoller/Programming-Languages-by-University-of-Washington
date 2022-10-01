(* Provided utility function *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1a *)

fun all_except_option (s, strings) = 
    let fun accumulate(scoped_strings, acc) =
          case scoped_strings of
            [] => NONE
          | [list_item] => if same_string(s, list_item) then SOME [] else NONE
          | list_head::list_tail => if same_string(s, list_head) 
            then SOME (acc @ list_tail)
            else accumulate(list_tail, acc @ [list_head])
    in
      accumulate(strings, [])
    end

(* 1b *)

fun get_substitutions1 (substitutions, s) =
  case substitutions of
      [] => []
    | [strings] =>  let val result = all_except_option(s, strings) 
                    in case result of
                        NONE => []
                      | SOME lst => lst
                    end
    | strings::strings' => get_substitutions1([strings], s) @ get_substitutions1(strings', s)    

(* 1c *)

fun get_substitutions2 (substitutions, s) =
  let 
    fun all_except(s, strings) =
      case all_except_option(s, strings) of
          NONE => []
        | SOME lst => lst  
    fun accumulate (scoped_substitutions, acc) =
      case scoped_substitutions of
          [] => acc
        | [strings] => acc @ all_except(s, strings) 
        | strings::strings' => accumulate(strings', acc @ all_except(s, strings))   
  in
    accumulate(substitutions, [])
  end

(* 1d *)

fun similar_names(substitutions, {first=first, middle=middle, last=last}) = 
  let 
    fun accumulate(names, acc) = 
      case names of
          [] => acc
        | [name] => acc @ [{first=name, middle=middle, last=last}]
        | name::names' => accumulate(names', acc @ [{first=name, middle=middle, last=last}])
  in
    accumulate(get_substitutions2(substitutions,first),[{first=first, middle=middle, last=last}])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2a *)

fun card_color(card) =
  case card of
      ((Spades | Clubs), _) => Black
    | ((Hearts | Diamonds), _) => Red

(* 2b (...or not 2b...) *)

fun card_value(card) =
  case card of
      (_,Ace) => 11
    | (_,(Jack | Queen | King)) => 10
    | (_,Num num) => num 

(* 2c *)

fun remove_card (cs, c, e) = 
  let
    fun accumulate(scoped_cs, acc) =
      case scoped_cs of
            [] => acc
          | [card] => if card = c then acc else raise e
          | card::cards' => if card = c then cards' @ acc else accumulate(cards', card :: acc)
  in
    accumulate(cs,[])
  end

(* 2d *)

fun all_same_color(cs) =
  case cs of
      [] => true
    | [card] => true
    | head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

(* 2e *)

fun sum_cards(cs) = 
  let fun accumulate(scoped_cs,acc) = 
    case scoped_cs of
        [] => acc
      | [card] => card_value(card) + acc
      | card::cards' => accumulate(cards', card_value(card) + acc) 
  in
    accumulate(cs,0)
  end

(* 2f *)

fun score(held_cards, goal) = 
  let
    val sum = sum_cards(held_cards)
    val prelim_score = if (sum > goal) then (3 * (sum - goal)) else (goal - sum)
  in
    if all_same_color(held_cards) then (prelim_score div 2) else prelim_score
  end

(* 2g *)

fun draw_card(remaining_moves, remaining_cards, remaining_held_cards, goal) =
  case remaining_cards of
      [] => ([], [], remaining_held_cards)
    | card::cards' => if (sum_cards(card :: remaining_held_cards) > goal) 
                      then ([], cards', card :: remaining_held_cards)
                      else (remaining_moves, cards', card :: remaining_held_cards)

fun handle_move(move, remaining_moves, remaining_cards, remaining_held_cards, goal) =
  case move of 
      Discard card => (remaining_moves, remaining_cards, remove_card(remaining_held_cards, card, IllegalMove))
    | Draw => draw_card(remaining_moves, remaining_cards, remaining_held_cards, goal)
  
fun officiate(cards, moves, goal) = 
  let 
    fun play(remaining_moves, remaining_cards, remaining_held_cards) = 
      case remaining_moves of
          [] => score(remaining_held_cards, goal)
        | move::moves' => play(handle_move(move, moves', remaining_cards, remaining_held_cards, goal)) 
  in
    play(moves, cards, [])
  end