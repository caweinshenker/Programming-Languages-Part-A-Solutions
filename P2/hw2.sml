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
      | x::[] => if same_string(x, s)
		 then []
		 else xs
      | x::xs' => if same_string(x, s)
		  then xs
		  else x::all_except_option(s, xs')

fun get_substitutions (list, s) =
  case list of
      [] => []
    | l::[] => all_except_option(s, l)
    | l::list' => all_exception_option(s, l) @ get_substitutions(list', s)

fun get_substitutions2 (list, s) =
  let fun get_sub_helper(list, s, acc) =
	case list of
	    [] => acc
	  | l::[] => get_sub_helper([], s, all_except_option(s, l) @ acc)
	  | l::list'=> get_sub_helper(list', s, all_exception_option(s, l) @ acc)
  in
      get_sub_helper(list, s, [])
  end

fun similiar_names (list, name) =
  let fun similar_helper(subs, name) =
	case subs of
	    [] => name::[]
	  | sub::_ => case name of
			  {first:x, middle:y, last:z} => {first:sub, middle:y, last:z}
  in
      case name of
	  {first: x, middle: y, last: z} =>
	  let val firstname_subs = get_substitutions2(list, first)
	  in similar_helper(firstname_subs, name)
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
      {1:_, 2:Clubs) => Black    
    | {1:_, 2:Spades} => Black
    | _ => Red
	       

fun card_value (card) =
  case card of
      {1:_, 2: Num(int)} => int
    | {1:_, 2: Ace} => 11
    | _ => 10


fun remove_card (cards, c, e) =
  let fun remove_helper(cards, c, e, removed) =
	case cards of
	    [] => if removed
		  then []
		  else raise e
	  | card::[] => if card = c and not removed
			then remove_helper([], c, e, true)
			else card::remove_helper([], c, e, removed)
	  | card::cards' => if card = c and not removed
			    then remove_helper(cards', c, e, true)
			    else card::remove_helper(cards', c, e, removed)
  in
      remove_helper(cards, c, e, false)
  end
      
		 
	       
fun all_same_color (cards) =
  case cards of
      [] => true
    | card::[] => true
    | card1::card2::[] => (card_color(card1) = card_color(card2)) and all_same_color([])
    | card1::card2::cards' = (card_color(card1) = card_color(card2)) and all_same_color(cards')


fun card_sum (cards) =
  let fun sum_helper(cards, acc) =
	case cards of
	    [] => acc
	  | card::[]  => sum_helper([], acc + card_value(card))
	  | card::cards' => sum_helper([], acc + card_value(cards')) 
  in
      sum_helper(cards, 0)
  end


      

																		       
											      
