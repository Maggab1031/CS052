(*
 * asgt06-examples.sml
 *
 * Rett Bull
 * Pomona College
 * June 21, 2017
 *
 * Here are some formulas, and functions that generate formulas, that you
 * can use for testing your DNF conversion functions. You can use them in
 * these ways.
 *
 * 1. Simply generate a formula and verify that it gives a valid DNF 
 * equivalent. (But remember that testing can only reveal defects; it 
 * cannot guarantee complete correctness.) Example:
 *
 *     val formula = exponentialFormula 5;
 *     val dnfFormula = expToDNF formula;
 *     checkEquivalence formula dnfFormula (* to verify equivalence           *)
 *     map length dnfFormula               (* should return a list of 32 10's *)
 *
 *
 * 2. Generate a short formula and examine the conversion. Explain to
 * yourself why the formulas are equivalent. Example:
 *
 *     expToDNF (circularFormula 3);
 *     (* should return a list of eight 3-element clauses, 
 *      * six of which are contradictory
 *      *)
 *     betterExpToDNF (circularFormula 3);
 *     (* ideally will return
 *      *  [[Not (Prop 0),Not (Prop 1),Not (Prop 2)],
 *      *   [Prop 1,Prop 2,Prop 0]]
 *      *)
 *
 *
 * 3. Generate successively longer formulas and observe the length of
 * time it takes to convert them and how long the results are.
 * Example:
 *
 *     expToDNF (oddParity 6);
 *     length it;    (* must be at least 32, probably much larger *)
 *     expToDNF (oddParity 7);
 *     length it;    (* over 300,000 without simplification *)
 *     expToDNF (oddParity 8);
 *     length it;
 *     ...           (* ... until you get tired *)
 *)

(* exponential formula:
 * 
 * (exponentialFormula k) is a formula that contains Props 0 through
 * 2k-1. The formula is true if exactly one member of each pair
 * (Propositon (2j), Prop (2j+1)) is true. There are k such pairs,
 * and so there are 2^k ways to make the formula true. An equivalent DNF
 * formula must have 2^k clauses.
 *)
fun exponentialFormula k = if k <= 0
                              then True
                           else if k = 1
                              then Xor (Prop 0, Prop 1)
                              else And(exponentialFormula (k-1),
                                       Xor (Prop (2*k-2),
                                            Prop(2*k-1)));


(* chain of implication:
 *
 * (tailFormula k) is a formula that contains Props 0 through k-1,
 * arranged in a chain of implications. If Prop j is true, then
 * all higher-numbered Props must also be true. There are k+1 ways
 * to make the formula true, but it is possible to create a DNF equivalent
 * with only k clauses.
 *)
fun tailFormula k =
    if k < 2
       then True
    else if k = 2
       then Implies(Prop 0, Prop 1)
    else And(tailFormula (k-1),
             Implies(Prop (k-2), Prop (k-1)));


(* circular implication:
 *
 * (circularFormula k) is a formula that contains Props 0 through k-1,
 * arranged in a cycle of implications. If one Prop is true, then so
 * are all the others. There are exactly two ways to make the formula true, 
 * but some conversions will list hundreds of clauses.
 *) 
fun circularFormula k = if  k < 2
                        then True
                        else And(tailFormula k,
                                 Implies(Prop (k-1), Prop 0));


(* odd parity:
 *
 * (oddParity k) is a formula that contains Props 0 through k-1.
 * The formula is true when the number true Props is odd. There
 * are 2^(k-1) ways to make the formula true. An equivalent DNF formula
 * will have at least 2^(k-1) clauses and may have many more.
 *
 * When applied to (oddParity 7), our naive reference solution yields a 
 * DNF formula with 335,877 clauses. Our more sophisticated reference
 * solution yields a formula with 64 clauses, the minimum number.
 *
 * Because the length of the DNF formula grows exponentially with k, 
 * (oddParity k) provides a good platform for experimentation. Our
 * naive solution will give a result for k=7 in a reasonable amount
 * of time (a few seconds), but we do not have the patience to wait
 * for the result when k=8. In contrast, our more sophisticated
 * solution will take us up to k=15 before we get tired of waiting.
 *)
fun oddParity k = if k <= 0
               then False
            else if k = 1
               then Prop 0
               else Xor (Prop (k-1), oddParity (k-1));


(* a random formula:
 *
 * This single formula contains Props 0 through 3. It was created
 * using a random number generator as a complicated example that 
 * you can use for exercising your functions. 
 *
 * Our experiments have yielded DNF formulas with as many as 7976
 * clauses, and as few as 4. For reference, here is the smallest
 * DNF equivalent that we have found:
 *     [[Not (Prop 0),Prop 1,Prop 3],
 *      [Not (Prop 1),Not (Prop 3)],
 *      [Not (Prop 2),Not (Prop 3)],
 *      [Prop 2,Prop 3]]
 *
 * A simpler, equivalent (non-DNF) formula is 
 *     (P2 <=> P3) Or ((P1 <=> P3) And (P1 => -P0))
 * or, expressed as a data structure,
 *     Or(Iff (Prop 2, Prop 3),
 *        And (Iff (Prop 1, Prop 3),
 *             Implies (Prop 1, Not(Prop 0))))
 *)
val randomFormula =
  Or
    (And
       (Xor (Prop 2,Prop 1),
        Implies
          (Implies
             (Implies
                (Implies (Prop 2,Prop 2),
                 And (Prop 3,Prop 0)),Prop 1),
           Implies
             (Iff
                (Or (Prop 3,Prop 2),
                 Iff (Prop 1,Prop 0)),
              Iff (Prop 2,Implies (Prop 3,Prop 3))))),
     Xor
       (Implies
          (Xor
             (Iff
                (Or (Prop 1,Prop 2),
                 And (Prop 2,Prop 1)),
              Implies
                (Or (Prop 2,Prop 1),
                 Or (Prop 1,Prop 2))),
           Not (Iff (Iff (Prop 2,Prop 3),Prop 1))),
        Or
          (And
             (Or (Xor (Prop 1,Prop 1),Prop 1),
              Iff (Not (Prop 2),Iff (Prop 3,Prop 3))),
           Not (Iff (Not (Prop 2),Not (Prop 3))))));


(* Pierce's formula
 *
 * A tautology, always true. But it is unlikely that your functions will generate
 * the absolutely simplest DNF equivalent.
 *
 *)
val piercesFormula = Implies (Implies (Implies (Prop 0,Prop 1),Prop 0),Prop 0);