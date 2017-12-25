(*
 * Gabe Magee
 *
 *
 * CS 52, Fall 2017
 * Assignment 6
 *
 *)

(*** Common material   $-* ***)
(* A directive to see formulas in all their glory *)
Control.Print.printDepth := 100;

(* Some utility functions that may be useful *)
fun member e = List.exists (fn x => x = e);

fun uniquify nil     = nil
  | uniquify (x::xs) =
    let
        val recResult = uniquify xs;
    in
        if member x recResult
           then recResult
           else x::recResult
    end;

fun cartesian nil     _       = nil
  | cartesian _       nil     = nil
  | cartesian [x]     (y::ys) = (x,y) :: (cartesian [x] ys)
  | cartesian (x::xs) yList   = (cartesian [x] yList) @ (cartesian xs yList);

(* The datatype from Bits and Logic *)
datatype booleanExpression =
           False
         | True    
         | Prop of int
         | Not of booleanExpression
         | And of booleanExpression * booleanExpression
         | Or of booleanExpression * booleanExpression
         | Xor of booleanExpression * booleanExpression
         | Implies of booleanExpression * booleanExpression
         | Iff of booleanExpression * booleanExpression;

(* The function evalExp from Bits and Logic *)
fun evalExp _      False                  = false
  | evalExp _      True                   = true
  | evalExp truths (Prop j)               = member j truths
  | evalExp truths (Not exp)              = not (evalExp truths exp)
  | evalExp truths (And (exp0, exp1))     =
        (evalExp truths exp0) andalso (evalExp truths exp1) 
  | evalExp truths (Or (exp0, exp1))      =
        (evalExp truths exp0) orelse (evalExp truths exp1) 
  | evalExp truths (Xor (exp0, exp1))     =
        (evalExp truths exp0) <> (evalExp truths exp1) 
  | evalExp truths (Implies (exp0, exp1)) =
        (not (evalExp truths exp0)) orelse (evalExp truths exp1)
  | evalExp truths (Iff (exp0, exp1))     =
        (evalExp truths exp0) = (evalExp truths exp1);

(* A handy function to avoid some double negations *)
fun negate False     = True
  | negate True      = False
  | negate (Not exp) = exp
  | negate exp       = Not exp;

(* A function to check your results *)
local
    fun isLiteral (Prop _)       = true
      | isLiteral (Not (Prop _)) = true
      | isLiteral _              = false;

    fun checkDNF dnfExpn = List.all (List.all isLiteral) dnfExpn;
    
    fun powerList nil     = [nil]
      | powerList (x::xs) =
            let
                val recResult = powerList xs;
            in
                recResult @ (map (fn set => x::set) recResult)
            end;

    fun subscriptsExp False                  = nil
      | subscriptsExp True                   = nil
      | subscriptsExp (Prop j)               = [j]
      | subscriptsExp (Not exp)              = subscriptsExp exp
      | subscriptsExp (And (exp0, exp1))     =
            (subscriptsExp exp0) @ (subscriptsExp exp1)
      | subscriptsExp (Or (exp0, exp1))      =
            (subscriptsExp exp0) @ (subscriptsExp exp1)
      | subscriptsExp (Xor (exp0, exp1))     =
            (subscriptsExp exp0) @ (subscriptsExp exp1)
      | subscriptsExp (Implies (exp0, exp1)) =
            (subscriptsExp exp0) @ (subscriptsExp exp1)
      | subscriptsExp (Iff (exp0, exp1))   =
            (subscriptsExp exp0) @ (subscriptsExp exp1);

    fun subscriptsDNF dnfExp =
        List.concat (map (List.concat o (map subscriptsExp)) dnfExp);
    
    fun evalDNF truths dnfExpn = List.exists (List.all (evalExp truths))
                                             dnfExpn;
    
in
    fun checkEquivalence boolExp dnfExp =
            let
                val subscripts = uniquify ((subscriptsExp boolExp) @
                                           (subscriptsDNF dnfExp));
	        val truthValues = powerList subscripts;
	        fun sameValue truths = (evalExp truths boolExp) =
                                       (evalDNF truths dnfExp);
            in
                if checkDNF dnfExp
                   then List.all sameValue truthValues
                   else (print "Not valid DNF!\n"; false)
            end;
end;


(*** Problem 06_01   $- $+06_01 ***)
(* elimConnectives : booleanExpression -> booleanExpression *)
fun elimConnectives True = True
  | elimConnectives False = False
  | elimConnectives (Prop j) = (Prop j)
  | elimConnectives (Not exp) = (Not (elimConnectives exp))
  | elimConnectives (And (exp0, exp1)) = (And ((elimConnectives exp0), (elimConnectives exp1)))
  | elimConnectives (Or (exp0,exp1)) =  (Or ((elimConnectives exp0),(elimConnectives exp1)))
  | elimConnectives (Xor (exp0, exp1)) = elimConnectives (Or ((And (exp0,(Not exp1))),(And ((Not exp0),exp1))))
  | elimConnectives (Implies (exp0, exp1)) = elimConnectives (Or ((Not exp0), exp1))
  | elimConnectives (Iff (exp0, exp1)) = elimConnectives (Or ((And (exp0,exp1)),(And ((Not exp0),(Not exp1)))));
                                

                              




(*** Problem 06_02   $-06_01 $+06_02 ***)
(* exception ConversionException                          *)
(* applyDeMorgan : booleanExpression -> booleanExpression *)
exception ConversionException;




fun applyDeMorgan True = True
  | applyDeMorgan False = False
  | applyDeMorgan (Prop j) = (Prop j)
  | applyDeMorgan (And (exp0,exp1)) = (And ((applyDeMorgan exp0),(applyDeMorgan exp1)))
  | applyDeMorgan (Or (exp0,exp1)) = (Or ((applyDeMorgan exp0),(applyDeMorgan exp1)))
  | applyDeMorgan (Xor (exp0,exp1)) = raise ConversionException
  | applyDeMorgan (Implies (exp0,exp1)) = raise ConversionException
  | applyDeMorgan (Iff (exp0,exp1)) = raise ConversionException
  | applyDeMorgan (Not exp0) =
                  
  let
      fun auxFunOne True = False
        | auxFunOne False = True
        | auxFunOne (Prop j) = (Not(Prop j))
        | auxFunOne (Not exp) = (applyDeMorgan exp)
        | auxFunOne (And ((exp0),(exp1)) ) = applyDeMorgan (Or ((Not exp0),(Not exp1)))
        | auxFunOne (Or ((exp0),(exp1)) ) = applyDeMorgan (And ((Not exp0),(Not exp1)))
        | auxFunOne (Xor (exp0,exp1)) = raise ConversionException
        | auxFunOne (Implies (exp0,exp1)) = raise ConversionException
        | auxFunOne (Iff (exp0,exp1)) = raise ConversionException;
  in
 (auxFunOne exp0)
                                    
  end;
  


                                                    
                                               




(*** Problem 06_03   $-06_02 $+06_03 ***)
(* toDNF : booleanExpression -> booleanExpression list list *)
fun auxFun nil = nil
  | auxFun (([x],[y])::xs) = ([x]@[y])::(auxFun xs)
  | auxFun (([x],nil)::xs) =([x])::(auxFun xs)
  | auxFun ((nil,[x])::xs) =([x])::(auxFun xs)
  | auxFun _ = raise ConversionException;


fun toDNF True = [[True]]
  | toDNF False = [[False]]
  | toDNF (Prop j) = [[Prop j]]          
  | toDNF (Not (Prop j)) = [[Not (Prop j)]]                   
  | toDNF (Or (exp0, exp1)) = (toDNF exp0)@(toDNF exp1)
  | toDNF (And (exp0, exp1)) =  auxFun(cartesian (toDNF exp0) (toDNF exp1))
  | toDNF _ = raise ConversionException;





(*** Problem 06_04   $-06_03 $+06_04 ***)
(* expToDNF : booleanExpression -> booleanExpression list list *)
fun expToDNF exp = toDNF(applyDeMorgan(elimConnectives (exp)));


(*** Problem 06_04   $-06_03 $+06_04 ***)
(* betterExpToDNF : booleanExpression -> booleanExpression list list *)
fun isNegation [x,y] = if (y = (Not(x))) orelse (x = (Not(y))) then false else true
  | isNegation [x] = true
  | isNegation _ = false;

(*
fun pointFour nil _ = false
  | pointFour _ nil = false
  | pointFour (x::xs) (y::ys) = if ((member Not(x) (y::ys))orelse (member Not(y) (x::xs)))andalso (xs = ys) then true
else pointFour xs ys ;
*)



fun betterExpToDNF exp = (List.filter isNegation(uniquify (map uniquify (expToDNF(exp)))));


(*** $-06_05 ***)