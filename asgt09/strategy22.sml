(*
 * strategy22.sml, a sample knuth_tree for Assignment 9
 *
 * Rett Bull
 * Computer Science 52
 * August 14, 2017
 *)

(* First, a check to see if the parameters match. *)
val _ = if (colorCount, pegCount) = (2,2)
           then ()
           else (print "\nSize mismatch!!\n";
                 raise InternalInconsistency);

(* Then, the tree. *)
val strategy22 =
  Step
    ([Red,Red],
     [Step ([Orange,Orange],[Lose,Lose,Lose,Lose,Win]),Lose,Lose,
      Step
        ([Red,Orange],
         [Lose,Lose,Step ([Orange,Red],[Lose,Lose,Lose,Lose,Win]),Lose,Win]),
      Win]);