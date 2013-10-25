val test1a = [
    all_except_option("foo", []) = NONE,
    all_except_option("foo", ["bar"]) = NONE,
    all_except_option("foo", ["foo"]) = SOME([]),
    all_except_option("foo", ["foo","x","y"]) = SOME(["x","y"]),
    all_except_option("foo", ["x","foo","y"]) = SOME(["x","y"]),
    all_except_option("foo", ["x","y","foo"]) = SOME(["x","y"])]


val test1b = [
    get_substitutions1([["Fred","Fredrick"],
  		["Elizabeth","Betty"],
			["Freddie","Fred","F"]], 
		       "Fred") = ["Fredrick","Freddie","F"],

    get_substitutions1([["Fred","Fredrick"],
			["Jeff","Jeffrey"],
			["Geoff","Jeff","Jeffrey"]],
		       "Jeff") = ["Jeffrey","Geoff","Jeffrey"]];
val test1c = [
    get_substitutions2([["Fred","Fredrick"],
			["Elizabeth","Betty"],
			["Freddie","Fred","F"]], 
		       "Fred") = ["Fredrick","Freddie","F"],

    get_substitutions2([["Fred","Fredrick"],
			["Jeff","Jeffrey"],
			["Geoff","Jeff","Jeffrey"]],
		       "Jeff") = ["Jeffrey","Geoff","Jeffrey"]];

val test1d = [
    similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
		  {first="Fred", middle="W", last="Smith"}) 
    =  [{first="Fred", last="Smith", middle="W"},
	{first="Fredrick", last="Smith", middle="W"},
	{first="Freddie", last="Smith", middle="W"},
	{first="F", last="Smith", middle="W"}]
	   ];


(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)


val test2a = [
    card_color(Clubs,Num(7)) = Black,
    card_color(Spades,Ace) = Black,
    card_color(Hearts,Queen) = Red,
    card_color(Diamonds,Num(4)) = Red];

val test2b = [
    card_value(Clubs,Num(7)) = 7,
    card_value(Spades,Ace) = 11,
    card_value(Hearts,Queen) = 10,
    card_value(Diamonds,Num(4)) = 4];

exception DonkeyKong;
val test2c = 
    let 
	val c1 = (Clubs,Num(7))
	val c2 = (Spades,Ace)
	val c3 = (Hearts,Queen)
	val c4 = (Diamonds,Num(4))
	val some_cards = [c1, c2, c3]
    in [
	remove_card(some_cards, c1, DonkeyKong) = [c2, c3],
	remove_card(some_cards, c2, DonkeyKong) = [c1, c3],
	remove_card(some_cards, c3, DonkeyKong) = [c1, c2],
	(remove_card(some_cards, c4, DonkeyKong) handle DonkeyKong => [c4]) = [c4],
	(remove_card([], c4, DonkeyKong) handle DonkeyKong => [c4]) = [c4]
    ]
    end;

val test2d = 
    let
    	val c1 = (Clubs,Num(7))
	val c2 = (Spades,Ace)
	val c3 = (Hearts,Queen)
	val c4 = (Diamonds,Num(4))
    in [
	all_same_color([]) = true,
	all_same_color([c1]) = true,
	all_same_color([c1,c2]) = true,
	all_same_color([c1,c2,c1,c2,c1,c2,c2]) = true,
	all_same_color([c1,c3]) = false,
	all_same_color([c4,c3]) = true,
	all_same_color([c4,c2,c1]) = false
    ] 
    end;

val test2e = 
    let
    	val c1 = (Clubs,Num(7))
	val c2 = (Spades,Ace)
	val c3 = (Hearts,Queen)
	val c4 = (Diamonds,Num(4))
    in [
	sum_cards([]) = 0,
	sum_cards([c1]) = 7,
	sum_cards([c1,c1,c1]) = 21,
	sum_cards([c2,c3]) = 21,
	sum_cards([c2,c3,c3,c3]) = 41,
	sum_cards([c4,c3,c2,c1]) = 32
    ]
    end;

val test2f = 
    let
    	val c1 = (Clubs,Num(7))
	val c2 = (Spades,Ace)
	val c3 = (Hearts,Queen)
	val c4 = (Diamonds,Num(4))
    in [
	score([], 10)   = 5,
	score([c3], 10) = 0,
	score([c3], 5)  = 7,
	score([c3,c2], 21) = 0,
	score([c3,c2], 25) = 4,
	score([c1,c2,c3,c4], 32) = 0,
	score([c1,c2,c3,c4], 20) = 36,
	score([c1,c2,c3,c4], 40) = 8
    ]
    end;

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end;
	
fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end;

	
val test2g = [
    (provided_test1() handle IllegalMove => ~1) = ~1,
    provided_test2() = 3
];
    (* officiate([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)],[Draw,Draw,Draw], 18)*)
			   


val test3a_1 = 
    let
    	val c1 = (Hearts,Ace)
	val c2 = (Spades,Ace)
    in [
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],99)=0,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],89)=0,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],53)=4,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],52)=3,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],51)=2,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],50)=1,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],49)=0,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],48)=3,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],47)=6,
	score_challenge([c1,c2,c2,c2,c2,c2,c2,c2,c2],46)=7
    ]
    end;



val test3a_2 = 
    let
	val cards= [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Hearts,Ace)]
	val moves1 =[Draw,Draw,Draw,Draw,Draw]

    in [
 	officiate_challenge(cards, moves1, 44) = 0,
 	officiate_challenge(cards, moves1, 43) = 3,
 	officiate_challenge(cards, moves1, 42) = 6,
 	officiate_challenge(cards, moves1, 41) = 7,
 	officiate_challenge(cards, moves1, 40) = 6
    ]
    end;


val test3b = 
    [
      careful_player([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 18),
      careful_player([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 8),
      careful_player([(Spades,Num 8),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 8),
      careful_player([(Spades,Num 8),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 18),
      careful_player([(Spades,Num 8),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 20),
      careful_player([(Spades,Num 8),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 21),
      careful_player([(Spades,Ace),(Hearts,Queen),(Spades,Num 7)], 21)

];
