(*
 * Gabe Magee
 *
 *
 * CS 52, Fall 2017
 * Assignment 7
 *
 *)

(*** Starter code   $-* $+ ***)
(*
 * Starter code, part 1 of 3, data types
 *
 * oper: the arithmetic operations
 *
 * token: the results of a lexical scan
 *
 * syntaxTree: the result of a parse operation
 *)
Control.Print.printLength := 200; (* default is 12 *)
Control.Print.printDepth := 30; (* default is 5 *)

datatype oper =
    Negate
  | Plus
  | Minus
  | Times
  | Divide
  | Remainder;

datatype token = 
    Number of int
  | Operation of oper
  | LParen
  | RParen
  | Semicolon;

datatype syntaxTree =
    Uninode of oper * syntaxTree
  | Binode of syntaxTree * oper * syntaxTree
  | Leaf of int;

(*
 * Starter code, part 2 of 3, exceptions
 *
 * When one of your functions encounters an error, it should raise an
 * exception with a descriptive message. In a top-level function, you 
 * will introduce code to handle the exceptions that are raised; see 
 * Problem~4.
 *)
exception LexicalException of string;
exception GrammarException of string;
exception CodeException of string;

(*
 * Starter code, part 3 of 3, an error-reporting utility
 *
 * These functions are for writing values out of the SML system. They are
 * executed not for the values they return, but for their side effects, and
 * so they violate the functional paradigm. We shall simply use them when 
 * necessary and not worry too much about the implementation.
 *
 * errorln: print an error message on the standard error stream, on a line
 * by itself. It returns nil because it is intended to be used with the 
 * function compile in Problem 4, which returns a list of strings.
 *)
val exitCode = ref OS.Process.success;
fun errorln str = (exitCode := OS.Process.failure;
                   TextIO.output (TextIO.stdErr, str ^ "\n");
                   TextIO.flushOut TextIO.stdErr;
                   nil);

(*** Problem 1   $- $+07_01 ***)
(* scan : char list -> token list *)
fun member e = List.exists (fn x => x = e);

exception RadixException;

fun charToDigit c =
    let
        fun auxFun _ nil _ = raise RadixException
          | auxFun x (c::cs) n = if x = c then n
            else auxFun x cs (n+1);
        val alpha = explode "0123456789ABCDEFGHJKLMNOPQRSTUVWXYZ";
    in
        auxFun c alpha 0
    end;


fun polyEval nil _ = 0
| polyEval (c::cs) x = c + x * (polyEval cs x);




fun scan nil = nil
  | scan (x::xs) = if (x = #"~") then ((Operation Negate)::(scan xs))
                    else if (x = #"+") then ((Operation Plus)::(scan xs))
                    else if (x = #"-") then ((Operation Minus)::(scan xs))
                    else if (x = #"*") then ((Operation Times)::(scan xs))
                    else if (x = #"/") then ((Operation Divide)::(scan xs))
                    else if (x = #"%") then ((Operation Remainder)::(scan xs))
                    else if (x = #"(") then ((LParen)::(scan xs))
                    else if (x = #")") then ((RParen)::(scan xs))
                    else if (x = #";") then ((Semicolon)::(scan xs))
                    else if (x = #" ") then (scan xs)
                    else if (Char.isDigit x) then (Number (polyEval(rev (numberAcc (x::xs))) 10))::(scan (tokenAcc(x::xs)))
                      handle Overflow => raise LexicalException "integer too large"
                    else raise LexicalException "illegal character"
and numberAcc nil = nil
  | numberAcc (x::xs) = 
    if (Char.isDigit x) then ((charToDigit x)::(numberAcc xs))
    else nil
and tokenAcc nil = nil
  | tokenAcc (x::xs) = if (Char.isDigit x) then (tokenAcc xs)
  else (x::xs);
                      



(*** Problem 2   $-07_01 $+07_02 ***)
(* parse : token list -> syntax tree *)


fun parse tknList = if (#2(expn tknList) = [Semicolon]) 
                    then #1(expn tknList) 
                else if (length (#2(expn tknList)) > 1) 
                    then raise  GrammarException "extra tokens"
                    else raise GrammarException "semicolon expected"
and expn lst = coExpn(term lst)
and coExpn (tr,((Operation Plus)::rest)) = 
                                          let
                                            val (rtree,rrest) = term rest;
                                          in
                                            coExpn (Binode(tr,Plus,rtree),rrest)
                                          end
  | coExpn (tr, ((Operation Minus)::rest)) =
                                          let
                                            val (rtree,rrest) = term rest;
                                          in
                                            coExpn (Binode(tr,Minus,rtree),rrest)
                                          end
  | coExpn (tr, rest) = (tr,rest)
and term lst = coTerm (factor lst)
and coTerm (tr, ((Operation Times)::rest)) =
                                            let
                                              val (rtree, rrest) = factor rest;
                                            in
                                              coTerm (Binode(tr, Times, rtree), rrest)
                                            end
  | coTerm (tr, ((Operation Divide)::rest)) =
                                            let
                                              val (rtree, rrest) = factor rest;
                                            in
                                               coTerm (Binode(tr, Divide, rtree), rrest)
                                            end
  | coTerm (tr, ((Operation Remainder)::rest)) =
                                            let
                                              val (rtree, rrest) = factor rest;
                                            in
                                               coTerm (Binode(tr, Remainder, rtree), rrest)
                                            end
  | coTerm (tr, rest) = (tr,rest)
and factor (Operation Negate::rest) =
                          let 
                            val (rtree, rrest) = (posFactor rest)
                          in
                            ((Uninode(Negate, rtree)),rrest)
                          end
    | factor tknList = (posFactor tknList)
and posFactor (LParen::rest) =
                                  let
                                      val (rtree, rrest) = expn rest
                                  in
                                      case rrest
                                          of (RParen :: rx) => (rtree, rx)
                                           | _              => raise GrammarException "right parenthesis expected"
                                  end
  | posFactor tknList = (numberFun tknList)
and numberFun ((Number x)::rest) = ((Leaf x), rest)
  | numberFun _ = raise GrammarException "number expected";

(*** Problem 3   $-07_02 $+07_03 ***)
(* encode : syntax tree -> string list *)
fun encodeAux (Leaf num) = ["  lcw r3 "^(Int.toString (num)),"  psh r3"]
  | encodeAux (Binode(left, Times, right)) = encodeAux left @ encodeAux right @["  pop r3","  lcw r2 mlmul","  cal r2 r2","  pop r0","  psh r3"]
  | encodeAux (Binode(left, Plus, right)) = encodeAux left @ encodeAux right @["  pop r3","  pop r2","  add r3 r2 r3","  psh r3"]
  | encodeAux (Binode(left, Minus, right)) = encodeAux left @ encodeAux right @["  pop r3","  pop r2","  sub r3 r2 r3","  psh r3"]
  | encodeAux (Binode(left, Divide, right)) = encodeAux left @ encodeAux right @["  pop r3","  lcw r2 dldiv","  cal r2 r2","  pop r0","  psh r3"]
  | encodeAux (Binode(left, Remainder, right)) = encodeAux left @ encodeAux right @["  pop r3","  lcw r2 dldiv","  cal r2 r2"]
  | encodeAux (Uninode(Negate, tree)) = ["  neg"]@ encodeAux tree 
  | encodeAux _ = raise CodeException "invalid syntax tree";

fun encode tree = ["BEGIN","  lcw r1 stack"]@(encodeAux tree)@["  pop r3","  sto r3 r0","  hlt","  inc mullib","  inc divilib","  dat 30","stack","END"];

(*** Problem 4   $-07_03 $+07_04 ***)
(* compile : string -> string list *)
fun compile str = (encode o parse o scan o explode) str
    handle LexicalException msg => errorln msg
         | GrammarException msg => errorln msg
         | CodeException msg    => errorln msg;



(*** Problem 5   $-07_04 ***)
(*
 * There is no code to write for Problem 5. Simply
 * invoke the driver file as described in the assignment.
 *)