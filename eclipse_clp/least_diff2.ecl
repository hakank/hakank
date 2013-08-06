/*

  Least diff problem in ECLiPSe.

  The program solves the following problem:
  
  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once, i.e.
  minimize the difference ABCDE - FGHIJ.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/least_diff.mzn
  * Choco   : http://www.hakank.org/choco/LeastDiff2.java
  * JaCoP   : http://www.hakank.org/JaCoP/LeastDiff.java
  * Gecode/R: http://www.hakank.org/gecode_r/least_diff.rb
  * Comet   : http://www.hakank.org/comet/least_diff.co
  * Gecode  : http://www.hakank.org/gecode/least_diff.cpp



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic), lib(ic_search), lib(ic_global), lib(branch_and_bound).

go  :-
        findall( [L2 - L1 = Diff], least_diff(L1, L2, Diff), L),
        write(L), nl.


least_diff(X, Y, Diff) :-
        LD = [A,B,C,D,E,F,G,H,I,J], % deklarera de 10 variablerna
        LD :: [0..9],
        Diff :: [0..1000000],

        % all numbers different
        ic_global:alldifferent(LD),

        % the constraints:
        %    A+B+C+D+E = F+G+H+I+J
        X #= 10000*A+1000*B+100*C+10*D+E,
        Y #= 10000*F+1000*G+100*H+10*I+J,

        % minimize the different
        X #< Y,
        Diff #= Y - X,

        % search
        minimize(search(LD,0,anti_first_fail,indomain_max,complete,[]),Diff).

