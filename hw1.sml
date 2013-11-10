exception WrongParameter;

(*  1  *)
fun is_older ((year, month, day) : int * int * int, (year', month', day') : int * int * int) : bool =
    if year < year' then true
    else if year = year' andalso month < month' then true
    else if year = year' andalso month = month' andalso day < day' then true
    else false;

(*  2  *)    
fun number_in_month ([], _ : int) : int = 0
  | number_in_month (((_ , month, _) :: dates) : (int * int * int) list , m : int) : int =
    let val re = number_in_month(dates, m)
    in 
        if month = m then
          re + 1
        else
          re
    end;
    
(*  3  *)
fun number_in_months ((dates : (int * int * int) list), []) : int = 0
  | number_in_months ((dates : (int * int * int) list), ((month : int) :: months)) : int = 
    number_in_month(dates, month) + number_in_months(dates, months);

(*  4  *)
fun dates_in_month ([], _:int) : (int * int * int) list = []
  | dates_in_month (((year , month, day) :: dates) : (int * int * int) list , m : int) : (int * int * int) list =
    let val re = dates_in_month (dates, m) in
        if month = m then
            (year, month, day) :: re
        else
            re
    end;
 
(*  5  *) 
fun dates_in_months (_:(int * int * int) list, []) : (int * int * int) list = []
  | dates_in_months (dates: (int * int * int) list , (m :: months) : int list) : (int * int * int) list =
    dates_in_month(dates, m) @ dates_in_months(dates, months)
    
(*  6  *) 
fun get_nth ([], _ : int) : string = raise WrongParameter
  | get_nth ((s::xs) : (string list), n : int) : string =
    if n = 1 then s
    else get_nth (xs, n-1);

(*  7  *)
fun date_to_string ((year, month, day) : int * int  * int) : string = 
    let val monthString = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] in
        get_nth(monthString, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year) 
    end;
(*  8  *)
fun number_before_reaching_sum (0, _) = 0
  | number_before_reaching_sum (sum : int, []) = raise WrongParameter
  | number_before_reaching_sum (sum : int, (x::xs) : int list) = 
    if sum > x then 
        1 + number_before_reaching_sum (sum-x, xs)
    else 0;

(*  9  *)     
fun what_month day : int = 
    if day >=1 andalso day <= 365 then
        let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
            number_before_reaching_sum (day, days_in_month)+1
        end
    else raise WrongParameter;

(*  10  *) 
fun month_range (day1 : int, day2 : int) : int list =
    if day1 > day2 then []
    else (what_month day1) :: month_range (day1+1, day2);

(*  11  *)
fun oldest ([]) : (int * int * int) option = NONE
  | oldest ((date :: xs) : (int * int * int) list) : (int * int * int) option = 
    let val oldest' = oldest xs
    in
        if oldest' = NONE then SOME date
        else
             let val oldest'' = valOf oldest'
             in
                if is_older(date, oldest'') then SOME date
                else SOME oldest''
             end   
    end;

(*  12  *)    
fun number_in_months_challenge ((dates : (int * int * int) list), (months : int list)) : int =
    let fun is_in_list ([], _) : bool = false
          | is_in_list ((x :: xs) :int list, y) : bool =
            if x = y then true
            else is_in_list (xs, y)
        fun unique ([]) : int list = []
          | unique ((x :: xs) : int list) : int list =
            let val xs' = unique (xs) in
                if is_in_list (xs, x) then
                    xs'
                else
                    x :: xs'
             end
         val months' = unique(months)
    in
         number_in_months(dates, months')
    end

fun dates_in_months_challenge ((dates : (int * int * int) list), (months : int list)) : (int * int * int) list =
    let fun is_in_list ([], _) : bool = false
          | is_in_list ((x :: xs) :int list, y) : bool =
            if x = y then true
            else is_in_list (xs, y)
        fun unique ([]) : int list = []
          | unique ((x :: xs) : int list) : int list =
            let val xs' = unique (xs) in
                if is_in_list (xs, x) then
                    xs'
                else
                    x :: xs'
             end
         val months' = unique(months)
    in
         dates_in_months(dates, months')
    end

(*  13  *)
fun reasonable_date ((year, month, day) : int * int * int) : bool =
    if year <= 0 then false
    else if month < 1 orelse month > 12 then false
    else  
    let fun getn ([], _ : int) = raise WrongParameter
          | getn ((x::xs) : int list, 1) : int = x
          | getn ((x::xs) : int list, n : int) : int = getn (xs, n-1)
        val is_leap = ((year mod 4) = 0 andalso (year mod 100) <> 0) orelse (year mod 400) = 0
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val days_in_this_month = 
            if is_leap = true andalso month = 2 then 29 
            else getn(days_in_month, month)
    in
        if day >=1 andalso day <= days_in_this_month then true
        else false
    end

(* ------------------------------------------------------------------------------------------------------ *)
(*  Homework1 Simple Test  *)
(*  These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader  *)
(*  To run the test, add a new line to the top of this file: use "homeworkname.sml";  *)
(*  All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool  *)


val test1 = is_older((1,2,3),(2,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3

val test9 = what_month(70) = 3

val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)    