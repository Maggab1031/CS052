(*
 * asgt07-driver.sml, a driver file for Assignment 7
 *
 * Rett Bull
 * Pomona College
 * January 5, 2017
 *
 * Instructions: Complete Problems 1 through 4 of
 * Assignment 7 in a file named asgt07.sml. Then
 * test your code by typing
 *
 *    sml asgt07-driver.sml | java -jar cs52-machine.jar -p -f
 *
 * at the command line. When you give it a correctly-
 * formed arithmetic string, it will compile it and
 * run the resulting program on the CS52 Machine.
 *
 *)

(* 
 * Bring in the solution to the assignment.
 *)
use "asgt07.sml";

(* 
 * printList, a function to print a list of strings, each one
 * on a line by itself
 *)
fun printList nil     = ()
  | printList (s::ss) = (print (s ^ "\n"); printList ss);

(*
 * readln, a function to read a string from the standard input
 *)
fun readln () =
    case TextIO.inputLine TextIO.stdIn of
         NONE   => (errorln "Input Error!!"; "")
       | SOME s => substring(s, 0, size s - 1);

(* 
 * The actual driver operations: read an expression, compile it,
 * print the resulting code, and quit.
 *)
val expression = readln();
printList(compile expression);
val _ = OS.Process.exit (!exitCode);