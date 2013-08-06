/*

  Torn number problem in SICStus Prolog.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/torn.html?19

  """
  The Torn Number from "Amusements in Mathematics, Dudeney", number 113

  I had the other day in my possession a label bearing the number 3025
  in large figures. This got accidentally torn in half, so that 30 was
  on one piece and 25 on the other. On looking at these pieces I began
  to make a calculation, scarcely concious of what I was doing, when I
  discovered this little peculiarity. If we add the 30 and the 25
  together and square the sum we get as the result the complete original
  number on the label! Now, the puzzle is to find another number,
  composed of four figures, all different, which may be divided in the
  middle and produce the same result. 
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/torn_number.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/torn_numbers.ecl
  
  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall([LD, Sum], torn(LD, Sum), L),
        % coverof([LD, Sum], torn(LD, Sum), L),
        write(L),nl.


torn(LD, Sum) :-
        LD = [D3, D2, D1, D0],
        domain(LD,0,9),

        all_different(LD),
        D3 #\= 0,
        Sum #= D3 * 10 + D2 + D1 * 10 + D0,
        Sum * Sum #= D3 * 1000 + D2 * 100 + D1 * 10 + D0,
        labeling([], LD).
