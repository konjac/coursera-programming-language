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

(* 1 *)
val only_capitals = List.filter (Char.isUpper o (fn s => String.sub(s, 0)));

(* 2 *)
val longest_string1 = foldl (fn (sa,sb) => if String.size(sa) > String.size(sb)  then sa else sb) "";

(* 3 *)
val longest_string2 = foldl (fn (sa,sb) => if String.size(sa) >= String.size(sb) then sa else sb) "";

(* 4 *)
fun longest_string_helper f str_list = foldl (fn (sa,sb) => if f(String.size(sa), String.size(sb)) then sa else sb) "" str_list;

val longest_string3 = longest_string_helper (op >);

val longest_string4 = longest_string_helper (op >=);

(* 5 *)
val longest_capitalized  = longest_string1 o only_capitals;

(* 6 *)
val rev_string = String.implode o rev o String.explode;

(* 7 *)
fun first_answer f lst =
    case lst of [] => raise NoAnswer
              | (x :: lst') => 
                    case f(x) of NONE => first_answer f lst'
                               | SOME v => v

(* 8 *)
fun all_answers f lst =
    let fun all_answers' f lst = foldl ( fn (x,y) => case (x,y) of (SOME x, SOME y) => SOME (y@x)
                                      | (_, _) => NONE) (SOME []) (map f lst)
    in 
        case lst of [] => SOME []
                  | lst => all_answers' f lst
    end;

(* 9 *)  
val count_wildcards = g (fn () => 1) (fn (x) => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

val count_some_var = fn (x, p) => g (fn () => 0) (fn (y) => if x = y then 1 else 0) p 

(* 10 *)
fun check_pat p =
    let fun all_var p =
       case p of Variable x        => [x]
               | TupleP ps         => List.foldl (fn (a,b) => a@b) [] (map all_var ps)
               | ConstructorP(_,p') => all_var p'
               | _ => []
        fun has_repeat lst =
            case lst of (s:: lst') => if (List.exists (fn (x) => x = s) lst') then true
                                      else has_repeat lst'
                      | _ => false
    in
        not ((has_repeat o all_var) p)
    end
    

(* 11 *)    
fun match (v, p) =
    case (p, v) of (Wildcard, _ ) => SOME []
                 | (Variable s, v) => SOME [(s, v)]
                 | (UnitP, Unit)  => SOME []
                 | (ConstP x, Const y) => if x = y then SOME [] else NONE
                 | (ConstructorP (s1,p), Constructor (s2,v)) => if s1 = s2 then match(v, p) else NONE
                 | (TupleP ps, Tuple vs) => if length(ps) <> length(vs) then NONE 
                                            else all_answers (fn (p,v) => match(v, p)) (ListPair.zip(ps, vs))
                 | (_, _) => NONE

(* 12 *)               
fun first_match v pattern_list =
    SOME (first_answer (fn p => match(v, p)) pattern_list)
    handle NoAnswer => NONE
    