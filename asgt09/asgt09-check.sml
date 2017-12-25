(*
 * asgt09-check.sml, a sanity-checker for Assignment 9
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
 *     sml asgt09-check.sml
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
use "asgt09.sml";

(*
 * Test each function by giving it an argument of the
 * appropriate type and comparing it to an incorrect
 * value of the appropriate result type. If there are
 * no type mismatch errors, the program will halt
 * normally.
 *)
val _ = nextMove (0,2) Win = Lose handle _ => false;
val _ = play (fn [Yellow,_,_,_] => (0,0) | _ => (4,0)) Win = [([Yellow,Red],(1,1))] handle _ => false;
val _ = knuthStrategy [[Red,Red],[Yellow,Red]] = Win handle _ => false;
val _ = maximumRounds Win = 12 handle _ => false;
val _ = expectedRounds Lose < 0.3 handle _ => false;
val _ = evilcodemaker [[Yellow]] [Red] = ((1,1),[[Blue],[Green]]) handle _ => false;

print "\nSanity check successfully passed!\n";
val _ = OS.Process.exit OS.Process.success;