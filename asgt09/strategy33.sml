(*
 * strategy33.sml, a sample knuth_tree for Assignment 9
 *
 * Rett Bull
 * Computer Science 52
 * August 14, 2017
 *)

(* First, a check to see if the parameters match. *)
val _ = if (colorCount, pegCount) = (3,3)
           then ()
           else (print "\nSize mismatch!!\n";
                 raise InternalInconsistency);

(* Then, the tree. *)
val strategy33 =
  Step
    ([Red,Red,Orange],
     [Step
        ([Yellow,Yellow,Yellow],[Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
      Step
        ([Orange,Yellow,Yellow],
         [Lose,Lose,Lose,Lose,Lose,
          Step
            ([Yellow,Yellow,Red],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Yellow,Orange,Yellow],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Orange,Orange,Yellow],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Win]),
      Step
        ([Orange,Yellow,Red],
         [Lose,Lose,Lose,Lose,Lose,Lose,
          Step
            ([Yellow,Orange,Red],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Orange,Orange,Red],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Win]),Lose,
      Step
        ([Red,Yellow,Yellow],
         [Step
            ([Orange,Orange,Orange],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Yellow,Orange,Orange],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Lose,Lose,
          Step
            ([Orange,Yellow,Orange],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Yellow,Yellow,Orange],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Yellow,Red,Yellow],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Lose,Win]),
      Step
        ([Red,Orange,Yellow],
         [Lose,Lose,
          Step
            ([Yellow,Red,Red],[Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Lose,Lose,
          Step
            ([Red,Yellow,Red],[Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Step
            ([Orange,Red,Yellow],
             [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Lose,Win]),
      Step
        ([Red,Orange,Red],
         [Lose,Lose,Lose,Lose,Lose,Lose,
          Step
            ([Orange,Red,Red],[Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Lose,Win]),
      Step
        ([Red,Red,Yellow],
         [Lose,Lose,Lose,Lose,
          Step
            ([Red,Orange,Orange],
             [Lose,Lose,Lose,Lose,Lose,Lose,
              Step
                ([Orange,Red,Orange],
                 [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Lose,Win]),
          Step
            ([Red,Yellow,Orange],
             [Lose,Lose,Lose,Lose,Lose,Lose,
              Step
                ([Yellow,Red,Orange],
                 [Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),Lose,Win]),
          Lose,
          Step ([Red,Red,Red],[Lose,Lose,Lose,Lose,Lose,Lose,Lose,Lose,Win]),
          Win]),Win])