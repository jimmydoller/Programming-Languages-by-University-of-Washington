fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 <> #1 date2
  then #1 date1 < #1 date2
  else if #2 date1 <> #2 date2
  then #2 date1 < #2 date2
  else #3 date1 < #3 date2

fun number_in_month(date_list : (int * int * int) list, month : int) =
  let
    fun count(lst : (int * int * int) list, cnt : int) =
      if null lst
      then cnt
      else if (#2 (hd lst)) = month
      then count(tl lst, cnt + 1)
      else count(tl lst, cnt)
  in
    count(date_list, 0)
  end

fun number_in_months(date_list : (int * int * int) list, month_list : int list) =
  let
    fun iterate(lst : int list, cnt : int) =
      if null lst
      then cnt
      else iterate(tl lst, cnt + number_in_month(date_list, hd lst))

    in
      iterate(month_list, 0)
  end

fun dates_in_month(date_list : (int * int * int) list, month : int) =
  if null date_list
  then []
  else if (#2 (hd date_list)) = month
  then (hd date_list)::dates_in_month(tl date_list, month)
  else dates_in_month(tl date_list, month)

fun dates_in_months(date_list : (int * int * int) list, month_list : int list) =
  if null month_list
  then []
  else
    dates_in_month(date_list, hd month_list) @
    dates_in_months(date_list, tl month_list)

fun get_nth(str_list : string list, n : int) =
  if n = 1
  then hd str_list
  else get_nth(tl str_list, n - 1)

fun date_to_string(date : (int * int * int)) =
  let
    val months =  ["January", "February", "March", "April",
      "May", "June", "July", "August", "September", "October", "November",
      "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^
      Int.toString (#1 date)
  end

fun number_before_reaching_sum(sum : int, int_list : int list) =
  let
    val new_sum = sum - (hd int_list)
  in
    if new_sum <= 0
    then 0
    else 1 + number_before_reaching_sum(new_sum, tl int_list)
  end

fun what_month(day_of_year : int) =
  let
    val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day_of_year, days_in_months) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else let
        fun max_date (dates : (int * int * int) list) =
          if null (tl dates)
          then hd dates
          else let val tl_max = max_date(tl dates)
               in
                 if is_older(hd dates, tl_max)
                 then hd dates
                 else tl_max
               end
       in
         SOME (max_date dates)
       end
