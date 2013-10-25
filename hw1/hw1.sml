(* problem 1 *)
fun is_older(date1:int*int*int, date2: int*int*int)=
	if #1 date1 > #1 date2 then false
	else if #1 date1 = #1 date2 then
		if #2 date1 > #2 date2 then false
		else if #2 date1 = #2 date2 then #3 date1 < #3 date2
		else true
	else true
		
(* problem 2 *)
fun number_in_month(dates: (int*int*int) list, month: int)=
	if null dates then 0
	else
		if #2 (hd dates) = month then
			1 + number_in_month(tl dates, month)
		else
			number_in_month(tl dates, month)
			
(* problem 3 *)
fun number_in_months(dates: (int*int*int) list, month_list: int list)=
	if null month_list then 0
	else number_in_month(dates, hd month_list) + number_in_months(dates, tl month_list)
	
(* problem 4 *)
fun dates_in_month(dates: (int*int*int) list, month)=
	if null dates then []
	else
		if #2 (hd dates) = month then
			(hd dates)::dates_in_month(tl dates, month)
		else
			dates_in_month(tl dates, month)

(* problem 5 *)
fun dates_in_months(dates: (int*int*int) list, month_list: int list)=
	if null month_list then []
	else dates_in_month(dates, hd month_list)@dates_in_months(dates, tl month_list)

(* problem 6 *)
fun get_nth(strs: string list, n: int)=
	if n = 1 then hd strs
	else get_nth(tl strs, n - 1)
	
(* problem 7 *)
fun date_to_string(date: int*int*int)=
	let val m = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in get_nth(m, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

(* problem 8 *)	
fun number_before_reaching_sum(sum: int, ints: int list)=
	if sum - hd ints <= 0 then 0
	else 1 + number_before_reaching_sum(sum - hd ints, tl ints)
	
(* problem 9 *)
fun what_month (day: int)=
	let val m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in number_before_reaching_sum(day, m) + 1
	end
	
(* problem 10 *)	
fun month_range (day1: int, day2: int)=
	if day1 > day2 then []
	else (what_month day1)::month_range(day1 + 1, day2)
	
(* problem 11 *)
fun oldest (dates: (int*int*int) list)=
	if null dates then NONE
	else
		let fun oldest_nonempty(ds: (int*int*int) list) =
			if null (tl ds) then hd ds
			else
				let val d = oldest_nonempty(tl ds)
				in
					if is_older(hd ds, d) then hd ds
					else d
				end
		in
			SOME (oldest_nonempty dates)
		end
	
(* problem 12 *)
fun contains(ele:int, ints:int list)=
	if null ints then false
	else
		if (hd ints) = ele then true
		else contains(ele, tl ints)
	
fun remove_duplicates(ints: int list)=
	if null ints then []
	else
		let val rest = remove_duplicates(tl ints)
		in if contains(hd ints, rest) then rest
			else (hd ints)::rest
		end
		
fun number_in_months_challenge(dates: (int*int*int) list, months: int list)=
	number_in_months(dates, remove_duplicates months)
	
fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) = 
	dates_in_months(dates, remove_duplicates months)

(* problem 13 *)
fun get_nth_month(strs: int list, n: int)=
	if n = 1 then hd strs
	else get_nth_month(tl strs, n - 1)

fun month_days(year:int, month: int)=
	let val m = 
		if (year mod 400 = 0) orelse (year mod 100 <> 0 andalso year mod 4 = 0) then
			[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		else
			[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		get_nth_month(m, month)
	end
			
fun reasonable_date(date: (int*int*int))=
	if #1 date < 1 then false
	else
		if #2 date < 1 orelse #2 date > 12 then false
		else
			if #3 date < 1 orelse #3 date > month_days(#1 date, #2 date) then false
			else true
			




