
 (*
 *
 *
 * CS 52, Fall 2017
 * Assignment 8
 *
 *)

(*** Starter code   $-* $+ ***)
(*
 * Starter code, part 1 of 5
 *
 * We want to be able to see long integers in their entirety,
 * so we set a parameter to a large value.
 *)
Control.Print.intinfDepth := 1000;

(*
 * Starter code, part 2 of 5
 *
 * We redeclare the usual arithmetic operators to work with IntInf.int. The
 * change is convenient in most situations, but it has the side effect of 
 * hiding the operators on ordinary int values. In the few cases when we 
 * want to work with ordinary int values, we must use awkward expressions
 * like Int.-(x,y).
 *
 * Notice the function divMod. The call divMod(x,y) returns an ordered pair
 * with both the quotient and remainder and is more efficient than calling
 * div and mod separately.
 *)
val op +    = IntInf.+;
val op -    = IntInf.-;
val op *    = IntInf.*;
val op div  = IntInf.div;
val op mod  = IntInf.mod;
val op <    = IntInf.<;
val op <=   = IntInf.<=;
val divMod  = IntInf.divMod;
val fromInt = IntInf.fromInt;
val toInt   = IntInf.toInt;

(*
 * Starter code, part 3 of 5
 *
 * Here are a few useful constants and a power-of-two function. Using zero 
 * in place of 0 helps the SML type inference system and helps to give you
 * functions with the proper type signatures.
 *)
val zero    = fromInt 0;
val one     = fromInt 1;
val two     = fromInt 2;
val three   = fromInt 3;

fun pow2 k  = IntInf.<<(one, Word.fromInt k);

(*
 * Starter code, part 4 of 5
 *
 * Here is a function to generate random IntInf.int values within a 
 * specificed range. Its results satisfy the condition
 *        low <= randomRangeIntInf (low,high) gen <= high
 *
 * To generate a non-zero random number with k bits, simply make the
 * call   randomRangeIntInf(one, pow2 k - one)
 *
 * See the assignment for more information about random number generators.
 *
 * randomRangeIntInf : int * int -> Random.rand -> IntInf.int
 *)
exception RandomIntInfError;
local
    fun randomBits b gen =
        let
            val bound = case b of
                             1 =>   1
                           | 2 =>   3
                           | 3 =>   7
                           | 4 =>  15
                           | 5 =>  31
                           | 6 =>  63
                           | 7 => 127
                           | 8 => 255
                           | _ =>   0;
        in
            fromInt(Random.randRange(0,bound) gen)
        end;
    fun randomIntInf bits gen =
        if Int.<=(bits,8)
           then randomBits bits gen
           else randomBits 8 gen +
                IntInf.<<(randomIntInf (Int.-(bits,8)) gen, Word.fromInt 8);
in
    fun randomRangeIntInf (low,high) gen =
        if high < low
           then raise RandomIntInfError
        else if low = high
           then low
           else
                let
                    val top = high - low;
                    val bitCount = Int.+(1,IntInf.log2 top);
                    fun trialValue () =
                        let
                            val trial = randomIntInf bitCount gen;
                        in
                            if trial <= top
                               then trial
                               else trialValue()
                        end;
                in
                    low + trialValue()
                end;
end;


(*
 * Starter code, part 5 of 5
 *
 * Here is a function for finding the mod-m multiplicative inverse of u. 
 * It will return SOME a, with 0 <= a < m, if ua mod m = 1. It will return
 * NONE if there is no such a. 
 *
 * The implementations uses a variant of Euclid's algorithm for finding
 * greatest common divisors. See the assignment for a discussion of the
 * theory behind the function.
 *
 * inverseMod : IntInf.int * IntInf.int -> IntInf.int option
 *)
fun inverseMod (u,m) =
        let
            (*
             * Invariant: There are constants b and d
             * satisfying
             *     x = au+bm  and  y = cu+dm.
             * If ever x=1, then 1 = au+bm, and a
             * is the inverse of u, mod m.
             *)
            fun extendedEuclid(x,y,a,c) =
                if x = zero
                   then NONE
                else if x = one
                   then SOME a
                else let
                         val (q,r) = divMod(y,x);
                         val newA = (c - a * q) mod m;
                     in
                         extendedEuclid(r,x,newA,a)
                     end;
        in
            extendedEuclid(u,m,one,zero)
        end;


(*** Problem 1   $- $+08_01 ***)
(* powerMod : IntInf.int * IntInf.int * IntInf.int -> IntInf.int *)
exception inputErr;

fun squareMod (k,n) = k * k mod n;

fun powerMod (b,e,n) = if e = zero then one
                       else if ((e mod two)=zero) andalso  e > zero then squareMod ((powerMod (b,(e div two),n)),n)
                       else ((squareMod ((powerMod (b,(e div two),n)),n))*b) mod n;




(*** Problem 2   $-08_01 $+08_02 ***)
(* block : IntInf.int * IntInf.int -> IntInf.int list   *)
(* unblock : IntInf.int * IntInf.int list -> IntInf.int *)
fun block (n,m) = if m = zero then [] else (m mod n)::(block(n,(m div n)));

fun unblock (_,nil) = zero
  | unblock (x,(c::cs)) = c + x * (unblock (x,cs));




(*** Problem 3   $-08_02 $+08_03 ***)
(* messageToIntInf : string -> IntInf.int *)
(* intInfToMessage : IntInf.int -> string *)
fun messageToIntInf str = unblock ((fromInt 256),(map fromInt (map ord (explode str))));

fun intInfToMessage n = implode (map chr (map toInt (block ((fromInt 256),n))));



(*** Problem 4   $-08_03 $+08_04 ***)
(* rsaEncode : IntInf.int * IntInf.int -> IntInf.int -> IntInf.int *)
fun rsaEncode (e,n) m = powerMod(m,e,n);

fun rsaDecode (d,n) c = powerMod(c,d,n);


(*** Problem 5a   $-08_04 $+08_05a ***)
(* encodeString : IntInf.int * IntInf.int -> string -> IntInf.int *)
fun encodeString (e,n) str =
    let
        val lst = block (n,(messageToIntInf str));
        fun auxFun (e,n) nil = nil
           |auxFun (e,n) (x::xs) = (rsaEncode (e,n) x):: (auxFun (e,n) xs);
    in 
        unblock (n,(auxFun (e,n) lst))
    end;


(*** Problem 5b   $-08_05a $+08_05b ***)
(* sampleEncryption : IntInf.int *)
val sampleEncryption = encodeString (fromInt 7, fromInt 111) "Don't panic and always carry your towel.";


(*** Problem 5c   $-08_05b $+08_05c ***)
(* decodeString : IntInf.int * IntInf.int -> IntInf.int -> string *)
(* sampleDecryption : string                                      *)
fun decodeString (d,n) c = 
    let
        fun auxFun (e,n) nil = nil
           |auxFun (e,n) (x::xs) = (rsaDecode (e,n) x):: (auxFun (e,n) xs);
    in 
        intInfToMessage(unblock (n,(auxFun (d,n) (block (n,c)))))
    end;

val sampleDecryption = decodeString (fromInt 31, fromInt 111) sampleEncryption;


(*** Problem 6a   $-08_05c $+08_06a ***)
(* generator : Random.rand *)
val generator = Random.rand (47,234);



(*** Problem 6b   $-08_06a $+08_06b ***)
(* industrialStrengthPrime : int -> Random.rand -> IntInf.int *)
fun industrialStrengthPrime k gen = 
  let 
    val randomNum = (randomRangeIntInf(pow2 (Int.-(k,1)), pow2 k - one) gen);
    val numList = List.tabulate (20, fn f => (randomRangeIntInf(pow2 (Int.-(k,1)), pow2 k - one) gen) mod randomNum)
    fun checkPrime x = powerMod(x, randomNum, randomNum) = x
  in
    if (List.all checkPrime numList) andalso (one < randomNum)
    then randomNum
    else industrialStrengthPrime k gen
  end;


(*** Problem 7   $-08_06b $+08_07 ***)
(* newRSAKeys : int -> Random.rand ->
                (IntInf.int * IntInf.int) * (IntInf.int * IntInf.int) *)
fun auxFun phin n gen = 
  let
    val d =  randomRangeIntInf (1,n) gen;
  in
    case inverseMod(d,phin) of
      SOME a => if((a mod phin) = d) then (auxFun phin n gen)  else (d,a)
      | NONE => auxFun phin n gen
  end;


fun newRSAKeys k gen = 
  let 
    val p = industrialStrengthPrime k gen;
    val q = industrialStrengthPrime k gen;
    val n = p*q;
    val phin = (p - one) * (q - one);
    val pair = (auxFun phin n gen);
    val d = #1(pair);
    val e = #2(pair);
  in
    ((e,n),(d,n))
  end;


(*** Problem 8   $-08_07 ***)
(* hidden, submitted as asgt08.rsa *)


(*"I have another encoded message hidden separately for which the following is a private key"^*)


(*** Problem 9   $+08_09 ***)
(* crackedPrivateKey : IntInf.int * IntInf.int *)
fun factor (a,b) = if a mod b = 0 then (b, a div b)
                else factor (a,b+1);

fun keyCracker (e,n)= 
  let
    val (p,q) = factor (n,2);
    val phin = (p-one)*(q-one);
    val d = randomRangeIntInf (two,n) generator;
  in
    if d*e mod phin = 1 then (d,n) else keyCracker (e,n)
  end;


val crackedPrivateKey = keyCracker(22821,52753);


(*** $-08_09 ***)

(*a 160 but answer would take t* 2^144 seconds to find *)