(*
 * Gabe Magee
 *
 *
 * CS 52, Fall 2017
 * Assignment 9
 *
 *)

(*** Configuration settings   $09_config+ ***)
(* You may change these for testing. The constraints are
 * 1 <= colorCount <= 9 and 1 <= pegCount.
 *)

val (colorCount, pegCount) = (6,4); 

(*** Starter code   $-* $+ ***)

(* Starter code I: datatypes *)
datatype peg = Red | Orange | Yellow | Green | Blue | Violet |
               Magenta | Turquoise | Celadon;

type code = peg list;

type response = int * int;

type codemaker = code -> response;

datatype knuth_tree = Lose
                    | Step of code * knuth_tree list
                    | Win;

(* Starter code II: configuration *)
local
    val fullSpectrum = [Red, Orange, Yellow, Green, Blue, Violet,
                        Magenta, Turquoise, Celadon];
    fun consAll ulist elt = map (fn u => elt::u) ulist;
    fun consAcross ulist elist = List.concat (map (consAll ulist) elist);
    fun possibilities elts k =
        if k < 0
           then []
        else if k = 0
           then [[]]
        else consAcross (possibilities elts (k-1)) elts;
    fun makePair i j = (i,j);
    (* makePairsWithI creates [(i,0),(i,1),...,(i,k-i) *)
    fun makePairsWithI k i = List.tabulate(k+1-i, makePair i);
    fun makeResponseList k = List.concat (List.tabulate(k-1, makePairsWithI k))
                             @ [(k-1,0),(k,0)];
in
    val allColors = List.take(fullSpectrum, colorCount);
    val allCodes = possibilities allColors pegCount : code list;
    val allResponses = makeResponseList pegCount : response list;
    val winningResponse = (pegCount,0) : response;
end;

(* Starter code III: miscellaneous values and settings *)
exception InternalInconsistency;
Control.Print.printLength := 1024;
Control.Print.printDepth := 32;

(* Starter code IV: two functions borrowed from Assignment 3 *)
(* matches : code -> code -> response            *)
(* isConsistent code -> response -> code -> bool *)
local
    fun exactMatches (secret:code) (guess:code) = 
           length(List.filter (fn (x,y) => x=y) (ListPair.zip (secret, guess)));
    fun countColors (theList:code) =
           map (fn c => length (List.filter (fn x => x=c) theList)) allColors;
    fun totalMatches  (secret:code) (guess:code) = 
           foldl (op +)
                 0
                 (map Int.min (ListPair.zip (countColors secret,
                                             countColors guess)));
in
    fun matches secret guess =
           let
               val totalM = totalMatches secret guess;
               val exactM = exactMatches secret guess;
           in
               (exactM, totalM - exactM) : response
           end;
end;

fun isConsistent guess response candSoln = matches candSoln guess = response;


(*** Problem 1   $- $+09_01 ***)
(* nextMove : response -> knuth_tree -> knuth_tree *)

fun auxFun x nil count = count
   |auxFun x (y::ys) count = if x = y then count else auxFun x ys (count+1);

fun nextMove res Win = raise InternalInconsistency
   |nextMove res Lose = raise InternalInconsistency
   |nextMove res (Step (guess,treeList)) =  List.nth (treeList,(auxFun res allResponses 0));





(*** Problem 2   $-09_01 $+09_02 ***)
(* play : codemaker -> knuth_tree -> (code * response) list *)
fun play (cdmkr:codemaker) (Lose:knuth_tree) = raise InternalInconsistency
  | play (cdmkr:codemaker) (Win:knuth_tree) = nil
  | play (cdmkr:codemaker) ((Step (guess,treeList)):knuth_tree) = 
      (guess,(cdmkr guess)):: (play (cdmkr) (nextMove ((cdmkr) guess) (Step (guess,treeList))));




(*** Problem 3   $-09_02 $+09_03 ***) 
(* knuthStrategy : code list -> knuth_tree *)
fun filterCodes guess possibleSecrets response =List.filter ( isConsistent guess response ) possibleSecrets;

fun greatestNum nil = raise InternalInconsistency
  | greatestNum (x::nil) = x
  | greatestNum (x::y::xs) = if x >=y then greatestNum (x::xs) else greatestNum (y::xs);

fun score lst guess= greatestNum (map List.length (map (filterCodes guess lst) allResponses));

fun everythingBut x lst = List.filter ( fn y => not (x = y)) lst;

fun guessesRemaining guess lst = map (everythingBut guess) (map (filterCodes guess lst) allResponses);

fun leastNumPos nil n = n
  | leastNumPos (x::nil)  n = n
  | leastNumPos (x::y::xs) n = if x <= y then leastNumPos (x::xs) n else leastNumPos (y::xs) n+1;

fun leastBad lst = List.nth (lst, (leastNumPos (map (score lst) lst) 0));

fun removeFinal lst = List.take (lst, (List.length lst)-1);

fun knuthStrategy nil = Lose
  | knuthStrategy (lst) = Step ((leastBad lst),((removeFinal(map knuthStrategy (guessesRemaining (leastBad lst) lst)))@[Win]));
    


(*** Problem 4   $-09_03 $+09_04 ***)
(* maximumRounds : knuth_tree -> int *)
(* expectedRounds : knuth_tree -> real *)
fun listOfPlays cdmkr nil tree = nil
  | listOfPlays cdmkr (x::xs) tree = (play (cdmkr x) tree)::listOfPlays cdmkr (xs) tree;

fun countr nil = nil
  | countr (x::xs) = (List.length x)::(countr xs);

fun averageElt nil total count = total/count
  | averageElt (x::xs) total count = averageElt xs (total + (Real.fromInt x)) (count+(Real.fromInt 1));


fun maximumRounds tree = greatestNum(countr (listOfPlays matches allCodes tree));

fun expectedRounds tree = averageElt (countr (listOfPlays matches allCodes tree)) (Real.fromInt 0) (Real.fromInt 0);




(*** Problem 5   $-09_04 $+09_05 ***)
(* evilcodemaker : code list -> code -> response * code list *)
fun greatestNumPos nil n = n
  | greatestNumPos (x::nil)  n = n
  | greatestNumPos (x::y::xs) n = if x >= y then greatestNumPos (x::xs) n else greatestNumPos (y::xs) n+1;


fun takeCandidates nil _ _ = nil
  | takeCandidates (x::xs) guess response = if isConsistent guess response x then (x:: takeCandidates xs guess response)
  else takeCandidates xs guess response;

fun matchResponse _ _ nil = nil
  | matchResponse candidates guess lst = map (takeCandidates candidates guess) lst;



fun evilcodemaker lst guess = 
  let
    val codeList = matchResponse lst guess allResponses
    val lengthList = map length codeList
    val greatestPos = greatestNumPos lengthList 0
    val greatestCode = List.nth (codeList, greatestPos)
    fun return nil _ _ = ((4,0):response,[guess])
      | return _ nil _ = ((4,0):response,[guess])
      | return (r::rs) (x::xs) y = if x = y then (r,y) else return rs xs y
  in
    return allResponses codeList greatestCode
  end;
  




(*** $-09_05 ***)