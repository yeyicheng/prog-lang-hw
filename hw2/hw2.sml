(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* problem 1-a *)
fun all_except_option (str: string, strs: string list)=
	case strs of 	
		  [] => NONE
		| x::strs' => if same_string(x, str) then
						SOME strs'
					else case all_except_option(str, strs') of
						  NONE => NONE
						| SOME y => SOME(x::y)

(* problem 1-b *)
fun get_substitutions1 (strls: string list list, s: string)=
	case strls of
		  [] => []
		| slist::xs => case all_except_option (s, slist) of
							  NONE => get_substitutions1(xs, s)
							| SOME y => y@get_substitutions1(xs, s)
									
(* problem 1-c *)	
fun get_substitutions2 (strls: string list list, s: string)=
	let fun aux(strls: string list list, s: string, acc: string list)=
		case strls of 
			  [] => acc
			| sl::xs => case all_except_option (s, sl) of 
						  NONE => aux(xs, s, acc)
						| SOME y => aux(xs, s, acc@y)
	in 
		aux (strls, s, [])
	end

(* problem 1-d *)
fun similar_names(strls: string list list, name: {first: string, middle: string, last: string})=
	let fun produce_names(first: string list, origin:{first: string, middle: string, last: string})=
		case first of 
		  [] => []
		| x::xs => case origin of
			{first=f, middle=m, last=l} => {first=x, middle=m, last=l}::produce_names(xs, origin)
	in
		case name of 
		{first=f, middle=m, last=l} => name::produce_names(get_substitutions1(strls, f), name)
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
(* problem 2-a *)
fun card_color(c: card)=
	case c of
		  (Clubs, _) => Black
		| (Spades, _) => Black
		|  _ => Red
		
(* problem 2-b *)
fun card_value(c: card)=
	case c of
		    (_, Num n) => n
		  | (_, Ace) => 11
		  | _ => 10
		  
(* problem 2-c *)
fun remove_card(cs: card list, c: card, e: exn)=
	case cs of 
		  [] => raise e
		| x::xs => if (x = c) then xs
					else x::remove_card(xs, c, e)
					
(* problem 3-d *)
fun all_same_color (cs: card list)=
	case cs of
		 [] => true
		| [x] => true 
		| (x::y::xs) => if (card_color(x) = card_color(y)) then
							all_same_color(y::xs)
						else false
						
(* problem 2-e *)
fun sum_cards(cs: card list)=
	let fun sum(cs': card list, s: int)=
		case cs' of
			  [] => s
			| x::xs => sum(xs, (card_value x) + s)
	in 
		sum(cs, 0)
	end
	
 (* problem 2-f *)
fun score(cs: card list, goal: int)=
	let val sum = sum_cards(cs) 
	in
		let val pre = 
			if sum > goal then
				3 * (sum - goal)
			else
				goal - sum
		in
			if (all_same_color cs) then
				pre div 2
			else
				pre
		end
	end
 
 (* problem 2-g *)
fun officiate(cs: card list, ms: move list, goal: int)=
	let fun curr(cs: card list, ms: move list, goal: int, hs: card list)=
		case ms of
			  [] => score(hs, goal)
			| x::xs => case x of
						  Discard c => curr(cs, xs, goal, remove_card(hs, c, IllegalMove))
						| Draw => case cs of
							  [] => score(hs, goal)
							| y::ys => if sum_cards(y::hs) > goal then
											score(y::hs, goal)
										else
											curr(remove_card(cs, y, IllegalMove), xs, goal, y::hs)
	in
		curr(cs, ms, goal, [])
	end
	
(* problem 3-a *)
fun score_challenge(cs: card list, goal: int)=
	let 
		fun num_of_ace (cards: card list, num)=
			case cards of
				  [] => num
				| x::xs => case x of
							  (_, Ace) => num_of_ace(xs, num + 1)
							| _ => num_of_ace(xs, num)
	in
		let 
			val sum11 = sum_cards(cs)
			val score11 = score(cs, goal)
			fun find_min(nofaces: int, sum: int, min: int)=
				case nofaces of
					  0 => min
					| aces => let val pre1 = 
									if sum - 10 > goal then
										3 * ((sum - 10) - goal)
									else
										goal - (sum - 10)
								val pre = 
									if (all_same_color cs) then
										pre1 div 2
									else
										pre1
							in
								if pre >= min then find_min(aces - 1, sum - 10, min)
								else find_min(aces - 1, sum - 10, pre)
							end
		in
			find_min(num_of_ace(cs, 0), sum11, score11)
		end
	end
	
fun officiate_challenge(cs: card list, ms: move list, goal: int)=
	let fun curr(cs: card list, ms: move list, goal: int, hs: card list)=
		case ms of
			  [] => score_challenge(hs, goal)
			| x::xs => case x of
						  Discard c => curr(cs, xs, goal, remove_card(hs, c, IllegalMove))
						| Draw => case cs of
							  [] => score_challenge(hs, goal)
							| y::ys => if sum_cards(y::hs) > goal then
											score_challenge(y::hs, goal)
										else
											curr(remove_card(cs, y, IllegalMove), xs, goal, y::hs)
	in
		curr(cs, ms, goal, [])
	end
	
(* problem 3b *)	
fun careful_player(card_list,goal) = 
    let 
        fun possible_to_discard(held_card: card list,c:card) =
            case held_card of
             [] => (false, (Clubs, Num 10)) 
             |x::xs => if score(c::remove_card(held_card, x, IllegalMove), goal) = 0
                       then (true,x)
                       else (false, (Clubs, Num 10)) 
   
        fun make_move(held_list: card list,card_list: card list,move_list: move list) =
            if score(held_list,goal) = 0
            then move_list
            else  
                 case card_list of
                      [] => move_list
                      |x::xs => case held_list of 
                                     [] => make_move(x::held_list,xs,move_list@[Draw])
                                    |y::ys => case  possible_to_discard(held_list, x) of
                                                    (true,x1) => move_list@[(Discard x1), Draw]
                                                   |_ => if ((goal - sum_cards(held_list) > 10) orelse
                                                             (sum_cards(x::held_list) <= goal))
                                                         then 
                                                               make_move(x::held_list,xs,move_list@[Draw])
                                                         else
                                                               make_move(x::held_list,xs,move_list@[Discard y])
    in                                                             
        make_move([],card_list,[])
    end 