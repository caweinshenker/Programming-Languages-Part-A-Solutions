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


fun only_capitals strings =
  List.filter (fn str => Char.isUpper(String.sub(str, 0))) strings

fun longest_string1 strings =
  List.foldl (fn (s1, s2)  =>  if String.size(s1) >= String.size(s2)
			    then s1
			    else s2)
	     ""
	     strings
  
fun longest_string2 strings =
  List.foldl (fn (s1, s2) => if String.size(s1) > String.size(s2)
			     then s1
			     else s2
	     )
	     ""
	     strings


fun longest_string_helper f =
  fn strings => List.foldl (fn (s1, s2) =>  if f (String.size(s1), String.size(s2))
					    then s1
					    else s2)
			   ""
			   strings

fun longest_string3 strings =
  longest_string_helper (fn (l1, l2) => l1 >= l2) strings

			
fun longest_string4 strings =
  longest_string_helper (fn (l1, l2) => l1 > l2) strings

			
fun longest_capitalized strings =
  (longest_string3 o only_capitals) strings

		  
fun rev_string strings =
  (String.implode o List.rev o String.explode) strings

fun first_answer f =
  fn list => case list of
		 [] => raise NoAnswer
	       | x::xs' => case (f x) of
			       SOME x => x
			     | NONE => first_answer f xs'
						    

(*
let val found =  (List.find(fn x => case (f x) of
						      NONE => false
						    | SOME x => true
					)
					list )
	     in
		 case found of
		     NONE => raise NoAnswer
		  | SOME found => found
	     end
*)		 
			  
fun all_answers f =
  fn list =>
     let fun all_so_far (acc, list') =
	   case list' of
	       [] => SOME []
	     | x::xs' => case (f x) of
			     NONE => NONE
			   | SOME x => all_so_far( [x] @ acc, xs')
     in
	 all_so_far([], list)
     end
	 
				    

		 
		
						  
    
    
    
  
		      
	   
	   
     	 
		     
		 
			    
		 	     
