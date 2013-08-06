/*

  Mrs Timpkin's Age problem in SICStus Prolog.

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.
 
  When the Timpkinses married eighteen years ago, Timpkins was three
  times as old as his wife, and today he is just twice as old as she.
  How old is Mrs. Timpkin? 
  """

  Compare with the following model:
  * ECLiPSe: http://www.hakank.org/eclipse/timpkin.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(LD, timpkin(LD),L),
        write(L),nl.


timpkin(LD) :-
        
        LD = [T, W],
        domain(LD,1,100),
        T - 18 #= 3 * (W - 18),
        T #= 2 * W,

        labeling([],LD).


