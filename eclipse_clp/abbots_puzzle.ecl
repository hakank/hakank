/*

  Abbot's Puzzle in ECLiPSe.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.

  If 100 bushels of corn were distributed among 100 people in such a
  manner that each man received three bushels, each woman two, and each
  child half a bushel, how many men, women, and children were there?

  Dudeney added the condition that there are five times as many women as
  men. That way, the solution becomes unique (otherwise, there are seven
  solutions). 
  """

  Compare with the following:
  * MiniZinc: http://www.hakank.org/minizinc/abpuzzle.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:-lib(ic).

go :- 
        findall(LD, abbot(LD), L),
        writeln(L)
        .

abbot(LD) :-
        LD = [M, W, C],
        LD :: 1..100,
        M + W + C #= 100,

        % Men: 3, Women: 2, Children: 1/2 = 2*100 (i.e. multiply with 2)
        % M * 6 + W * 4 + C #= 200,
        % Note: Using ECLiPSe's ic, we don't have to multiply with 2.
        M * 3 + W * 2 + C/2 #= 100,
        M * 5 #= W,    % additional condition added by Dudeney      

        labeling(LD).




