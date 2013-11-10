(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* 1.  ============================================================== *)

(* (a) -------------------------------------------------------------- *)
fun all_except_option (  _ : string, [] ) : string list option = NONE 
  | all_except_option (str : string, (head :: lst) : string list) : string list option =
    let val tail = all_except_option(str, lst)
        val is_same = same_string(str,head)
    in
        case tail
          of NONE => if is_same then SOME lst else NONE
           | SOME lst' => if is_same then tail else SOME (head :: lst')
    end;
    
(* (b) -------------------------------------------------------------- *)

fun get_substitutions1 ( [] : string list list, str : string) : string list = []
  | get_substitutions1 ( (str_lst :: lst) : string list list, str : string) : string list =
    let val head = all_except_option (str, str_lst)
        val tail = get_substitutions1(lst, str)
    in  case head
          of NONE => tail
           | SOME lst' => (lst' @ tail)
    end;
    
(* (c) -------------------------------------------------------------- *)

fun get_substitutions2 ( lst : string list list, str : string) : string list = 
    let fun get_substitutions2' (res : string list, [], _ : string) : string list = res
          | get_substitutions2' (res : string list, (str_lst :: lst) : string list list, str : string) = 
                let val head = all_except_option (str, str_lst)
                in
                    case head
                      of NONE => get_substitutions2' (res, lst, str)
                       | SOME str_lst' => get_substitutions2' (res @ str_lst', lst, str)
                end
     in
        get_substitutions2'([], lst, str)
     end;

(* (d) -------------------------------------------------------------- *)
fun similar_names (str_lst_lst : string list list, {first : string, middle : string, last : string})
    : { first : string, middle : string, last : string } list = 
    let val simliar_first_names = get_substitutions2 (str_lst_lst, first)
        fun substitution_frist_names ( [], {first : string, middle : string, last : string}) : { first : string, middle : string, last : string } list = []
          | substitution_frist_names ((first' :: firsts) : string list, {first : string, middle : string, last : string}) : { first : string, middle : string, last : string } list =
            {first = first', middle = middle, last = last} :: substitution_frist_names (firsts, {first = first, middle = middle, last = last})
    in
        {first = first, middle = middle, last = last} :: substitution_frist_names (simliar_first_names, {first = first, middle = middle, last = last})
    end;

(* 2.  ============================================================== *)

(* (a) -------------------------------------------------------------- *)
fun card_color (c : card) : color =
    case c
        of (Clubs, _) => Black
         | (Spades, _) => Black
         | (Hearts, _) => Red
         | (diamonds, _) => Red;

(* (b) -------------------------------------------------------------- *)

fun card_value (c : card) : int =
    case c
        of (_, Num num) => num
         | (_, Ace) => 11
         | (_, _) => 10;

(* (c) -------------------------------------------------------------- *)
 
fun remove_card ([], c : card, e : exn) : card list = raise e
  | remove_card ((c' :: cs) : card list, c : card, e : exn) : card list =
        if c' = c then cs
        else c' :: remove_card(cs, c, e);
 
(* (d) -------------------------------------------------------------- *)

fun all_same_color ([]) : bool = true
  | all_same_color ((c :: []) : card list) = true
  | all_same_color ((c :: c' :: cs) : card list) =
        if card_color(c) = card_color(c') then all_same_color(c'::cs)
        else false;

(* (e) -------------------------------------------------------------- *)

fun sum_cards (cs : card list) : int =
    let fun sum_cards' (sum : int, []) : int = sum
          | sum_cards' (sum : int, (c :: cs) : card list) : int = sum_cards'(sum + card_value(c), cs)
    in
        sum_cards'(0, cs)
    end;
        
(* (f) -------------------------------------------------------------- *)

fun score (held_cards : card list, goal : int) = 
    let val sum = sum_cards(held_cards)
        val is_same_color = all_same_color(held_cards)
        val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if is_same_color then preliminary_score div 2
        else preliminary_score
    end;
    
(* (g) -------------------------------------------------------------- *)

fun officiate (cs : card list, moves : move list, goal : int) : int = 
    let fun officiate' (held_cards : card list, cs : card list, moves : move list, goal : int) : card list =
        if sum_cards(held_cards) > goal then held_cards
        else case moves
           of [] => held_cards
            | (Discard c) :: moves' => officiate'(remove_card(held_cards, c, IllegalMove), cs, moves', goal)
            | Draw :: moves' =>
                case cs of [] => held_cards
                         | (c :: cs') => officiate'(c::held_cards, cs', moves', goal)
    in
        score(officiate'([], cs, moves, goal), goal)
    end;


(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option("string", ["string"]) = SOME []

val test2 = get_substitutions1([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color((Clubs, Num 2)) = Black

val test6 = card_value((Clubs, Num 2)) = 2

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)