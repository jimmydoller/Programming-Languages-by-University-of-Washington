(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


fun all_except_option (str, str_lst) =
  let fun accu(saved, tl_lst) =
    case tl_lst of
         [] => NONE
       | x::t => if same_string(x, str)
                 then SOME (saved @ t)
                 else accu(x::saved, t)
  in
    accu([], str_lst)
  end

fun get_substitutions1(subs, s) =
  case subs of
       [] => []
     | hd_lst::tl_lst => case all_except_option(s, hd_lst) of
                              NONE => get_substitutions1(tl_lst, s)
                            | SOME lst => lst @ get_substitutions1(tl_lst, s)

fun get_substitutions2(subs, s) =
  let fun save_strings(curr, saved) =
    case curr of
         [] => saved
       | hd_lst::tl_lst => case all_except_option(s, hd_lst) of
                                NONE => save_strings(tl_lst, saved)
                              | SOME lst => save_strings(tl_lst, saved @ lst)
  in
    save_strings(subs, [])
  end

(* fun get_substitutions3(hd::tl, s) =
  case all_except_option(s, hd) of
       NONE => get_substitutions1(tl, s)
     | SOME lst => lst @ get_substitutions1(tl, s) *)

fun similar_names(subs, {first=f, middle=m, last=l}) =
  let fun help(sub_list) =
    case sub_list of
         [] => []
       | hd::tl => {first=hd, middle=m, last=l} :: help(tl)
  in
    {first=f, middle=m, last=l} :: help(get_substitutions2(subs, f))
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

fun card_color card =
  case card of
       (Clubs, _) => Black
     | (Spades, _) => Black
     | _ => Red

fun card_value card =
  case card of
       (_, Num x) => x
     | (_, Ace) => 11
     | _ => 10


fun remove_card (cs, c, e) =
  case cs of
       [] => raise e
     | hd::tl => if hd = c
                 then tl
                 else hd :: remove_card(tl, c, e)


fun all_same_color (crd_lst) =
  case crd_lst of
       [] => true
     | _::[] => true
     | top::(next::rest) => (card_color top = card_color next andalso
                              all_same_color(next::rest))


fun sum_cards (crd_lst) =
  let fun sum(lst, acc) =
    case lst of
         [] => acc
       | hd::tl => sum(tl, (card_value hd + acc))
  in
    sum(crd_lst, 0)
  end


fun score (crd_lst, goal) =
  let val sum = sum_cards crd_lst
      val prelim = if sum <= goal
                   then goal - sum
                   else (sum - goal) * 3
  in
    if all_same_color (crd_lst)
    then if prelim = 0 then 0 else (prelim div 2)
    else prelim
  end


fun officiate(crd_lst, move_lst, goal) =
  let fun run(held, deck, moves) =
        case moves of
             [] => score(held, goal)
           | Discard c :: cont => run(remove_card (held, c, IllegalMove), deck, cont)
           | Draw :: cont => case deck of
                                  [] => score(held, goal)
                                | top::rest => let val s = score(top::held, goal)
                                               in
                                                 if s > goal
                                                 then s
                                                 else run(top::held, rest, cont)
                                               end
      in
        run([], crd_lst, move_lst)
      end
