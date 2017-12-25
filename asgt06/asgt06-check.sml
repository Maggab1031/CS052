(*
 * asgt06-check.sml, a sanity-checker for Assignment 6
 *
 * Rett Bull
 * Pomona College
 * August 15, 2017
 *
 * This file is used as a simple check before submitting
 * the assignment. It verifies that the required functions
 * are present and have the correct type signature. It
 * does NOT check the functions for correctness.
 *
 * Usage: On the command line, type
 *
 *     sml asgt06-check.sml
 *
 * If you see an error, there is a problem with your solution.
 * Otherwise, your solution passes the (minimal) sanity
 * check.
 *)

(*
 * Avoid autoloading messages at the end.
 *)
val _ = OS.Process.exit;

(* 
 * Bring in the solution to the assignment.
 *)
use "asgt06.sml";

(*
 * Test each function by giving it an argument of the
 * appropriate type and comparing it to an incorrect
 * value of the appropriate result type. If there are
 * no type mismatch errors, the program will halt
 * normally.
 *)
val _ = elimConnectives False = True handle _ => false;
val _ = applyDeMorgan False = True handle _ => false;
val _ = toDNF False = [[Prop 0], [Not (Prop 1)]] handle _ => false;
val _ = expToDNF False = [[Prop 0], [Not (Prop 1)]] handle _ => false;
val _ = betterExpToDNF False = [[Prop 0], [Not (Prop 1)]] handle _ => false;

val _ = print "\nSanity check successfully passed!\n";
val _ = OS.Process.exit OS.Process.success;