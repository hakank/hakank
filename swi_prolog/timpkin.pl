/*

  Mrs Timpkin's Age in SWI Prolog

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.
 
  When the Timpkinses married eighteen years ago, Timpkins was three
  times as old as his wife, and today he is just twice as old as she.
  How old is Mrs. Timpkin? 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        findall(LD, timpkin(LD),L),
        writeln(L),
        nl.


timpkin([mr_timpkin=T,mrs_timpkin=W]) :-
   
   LD = [T, W],
   LD ins 1..100,
   T - 18 #= 3 * (W - 18),
   T #= 2 * W,

   label(LD).
