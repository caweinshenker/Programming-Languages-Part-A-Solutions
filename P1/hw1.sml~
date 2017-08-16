
(* HW1 SML *)

fun is_older(date1 : int*int*int, date2 : int*int*int) = 
    if (#1 date1) < (#1 date2)
    then true 
    else if (#2 date1) < (#2 date2)
    then true
    else if (#3 date1) < (#3 date2)
    then true
    else false
		     

fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if ( (#2 (hd dates)) = month)
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)


fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, (tl months))


fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if ( (#2 (hd dates)) = month)
  then (hd dates) :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)
		     
				 				   
fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

							 				   
fun get_nth(strings : string list, n : int) =
  if (n = 1)
  then hd strings
  else get_nth(tl strings, n - 1)

fun date_to_string(date : int * int * int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
      val month = get_nth(months, #2 date);
  in month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end
      

fun number_before_reaching_sum(sum : int, nums: int list) =
  let fun number_before_reaching_sum_helper(nums : int list, target : int, total : int, n : int) =
	if total + (hd nums) < target 
	then number_before_reaching_sum_helper(tl nums, target, total + (hd nums), n + 1)
	else n
  in number_before_reaching_sum_helper(nums, sum, 0, 0)
  end


fun what_month(day : int) =
  let val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in number_before_reaching_sum(day, month_lengths) + 1
  end

      
fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

				      
fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else
      let val oldest_date = oldest(tl dates);
	  val head = hd dates;
      in if isSome oldest_date andalso (is_older(valOf oldest_date, head))
	 then oldest_date
	 else SOME (head)
      end
	  
