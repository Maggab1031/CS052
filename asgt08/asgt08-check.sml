(*
 * asgt08-check.sml, a sanity-checker for Assignment 8
 *
 * Rett Bull
 * Pomona College
 * January 6, 2017
 *
 * This file is used as a simple check before submitting
 * the assignment. It verifies that the required functions
 * are present and have the correct type signature. It
 * does NOT check the functions for correctness.
 *
 * Usage: On the command line, type
 *
 *     sml asgt08-check.sml
 *
 * If you see an error, there is a problem with your solution.
 * Otherwise, your solution passes the (minimal) sanity
 * check.
 *)

(* 
 * Bring in the solution to the assignment.
 *)
use "asgt08.sml";

(*
 * Test each function by giving it an argument of the
 * appropriate type and comparing it to an incorrect
 * value of the appropriate result type. If there are
 * no type mismatch errors, the program will halt
 * normally.
 *)
val _ = powerMod(two,three,three) < zero handle _ => false;
val _ = block(fromInt 47, fromInt 23) = [fromInt 100] handle _ => false;
val _ = unblock(fromInt 47, [fromInt 100]) = fromInt 36 handle _ => false;
val _ = messageToIntInf "Bugs Bunny" = fromInt 1024 handle _ => false;
val _ = intInfToMessage (fromInt 1024) = "Elmer Fudd" handle _ => false;
val _ = rsaEncode(fromInt 7, fromInt 111) (fromInt 3) = fromInt 19
        handle _ => false;
val _ = encodeString(fromInt 7, fromInt 111) "Wiley Coyote" = fromInt 19
        handle _ => false;
val _ = sampleEncryption = fromInt 77 handle _ => false;
val _ = decodeString(fromInt 7, fromInt 111) (fromInt 23) = "" handle _ => false;
val _ = sampleDecryption = "Roadrunner" handle _ => false;
val _ = generator = Random.rand(1,2) handle _ => false;
val _ = industrialStrengthPrime 5 generator = three handle _ => false;
val _ = newRSAKeys 5 generator = ((fromInt 7,fromInt 9),(fromInt 31,fromInt 6))
        handle _ => false;
val _ = crackedPrivateKey = (fromInt 22, fromInt 111) handle _ => false;

val _ = print "\nSanity check successfully passed!\n";
val _ = OS.Process.exit OS.Process.success;
