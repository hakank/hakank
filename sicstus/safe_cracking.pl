/*

  Safe cracking problem in SICStus Prolog.

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The code of Professor Smart's safe is a sequence of 9 distinct 
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

 
   C1 <> 1, C2 <> 2, ..., C9 <> 9

   can you find the correct combination?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/safe_cracking.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/safe_cracking.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        findall(LD, safe(LD), L),
        write(L),nl.

safe(LD) :-
        LD=[C1, C2, C3, C4, C5, C6, C7, C8, C9],
        domain(LD,1,9),
        all_different(LD),
         
        % C1 <> 1, C2 <> 2, ..., C9 <> 9
        ( foreach(L,LD),
          count(I,1,_) do
              L #\= I
        ),
        
        C4 - C6 #= C7,
        C1 * C2 * C3 #= C8 + C9,
        C2 + C3 + C6 #< C8,
        C9 #< C8,

        labeling([], LD).
