(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, xs) = 
    case xs of
	[] => NONE
      | x::xs' => if same_string(x, s)
		  then SOME xs'
		  else case all_except_option(s, xs') of
			   NONE => NONE 
			 | SOME tail => SOME (x::tail)
					   

fun get_substitutions1 (list, s) =
  case list of
      [] => []
    | l::list' => case all_except_option(s, l) of
		    NONE => [] 
		   | SOME sublist => sublist @ get_substitutions1(list', s)


fun get_substitutions2 (list, s) =
  let
      fun extract_list_val(opt) =
	case opt of
	    NONE => []
	  | SOME sublist => sublist
      fun get_sub_helper(list, s, acc) =
	case list of
	    [] => acc
	  | l::list'=> get_sub_helper(list', s, acc @ extract_list_val(all_except_option(s, l)))
  in
      get_sub_helper(list, s, [])
  end


fun similar_names (list, name) =
  let fun similar_helper(subs, name) =
	case subs of
	    [] => []
	  | sub::subs' => case name of
			      {first=x, last=y, middle=z} =>
			      [{first=sub, last=y, middle=z}] @ similar_helper(subs', name)
  in
      case name of
	  {first=x, middle=y, last=z} =>
	  let
	      val firstname_subs = get_substitutions2(list, x)
	  in
	      name::similar_helper(firstname_subs, name)
	  end
  end
      
			   
	     
(* YOU may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color (card) =
  case card of
      {1=Clubs,  2=_} => Black    
    | {1=Spades, 2=_} => Black
    | _ => Red



fun card_value (card) =
  case card of
      {1=_ , 2=Num(int)} => int
    | {1=_ , 2=Ace} => 11
    | _ => 10


fun remove_card (cards, c, e) =
  let fun remove_helper(cards, c, e, removed) =
	case cards of
	    [] => if removed
		  then []
		  else raise e
	  | card::cards' => if card = c andalso removed = false
			    then remove_helper(cards', c, e, true)
			    else card::remove_helper(cards', c, e, removed)
  in
      remove_helper(cards, c, e, false)
  end
      	 
	       
fun all_same_color (cards) =
  case cards of
      [] => true
    | card::[] => true
    | card1::card2::cards' => (card_color(card1) = card_color(card2)) andalso all_same_color(card2::cards')

											    
fun sum_cards (cards) =
  let fun sum_helper(cards, acc) =
	case cards of
	    [] => acc
	  | card::cards' => sum_helper(cards', acc + card_value(card)) 
  in
      sum_helper(cards, 0)
  end



fun score (cards, goal) =
  let
      val sum = sum_cards(cards)
      val prelim_score = if sum > goal
			 then 3 * (sum - goal)
			 else (goal - sum)
				  
  in
      if all_same_color(cards)
      then prelim_score div 2
      else prelim_score	       
  end


fun officiate (cards, moves, goal) =
  let
      fun play (held, goal, cards, moves) =
	if sum_cards(held) > goal
	then score(held, goal)
	else case moves of
		 [] => score(held, goal)
	       | move::moves' => case move of
				     Discard(card) => play(remove_card(held, card, IllegalMove), goal, cards, moves')
				   | Draw => case cards of
						 [] => score(held, goal)
					       | card::[] => play(card::held, goal, [], moves')
					       | card::cards' => play(card::held, goal, cards', moves')
							   
  in
      play([], goal, cards, moves)
  end   
	
  
		  
      

																		       
											      
