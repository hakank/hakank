/*
  Least diff problem in SICStus Prolog.

  The program solves the following problem:

  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once.

  Compare with the following models:

  Choco: http://www.hakank.org/choco/LeastDiff2.java
  ECLiPSE: http://www.hakank.org/eclipse/least_diff2.ecl
  Comet http://www.hakank.org/comet/least_diff.co
  Essence': http://www.hakank.org/tailor/leastDiff.eprime
  Gecode: http://www.hakank.org/gecode/least_diff.cpp
  Gecode/R: http://www.hakank.org/gecode_r/least_diff.rb
  JaCoP: http://www.hakank.org/JaCoP/LeastDiff.java
  MiniZinc: http://www.hakank.org/minizinc/least_diff.mzn


  Note: This exact model was one of my very first things
        in constraint (logic) programming.
  
  This model was created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/
 
*/
 
:-use_module(library(clpfd)).


go  :-  
        findall( [L2 - L1 = Diff], least_diff(L1, L2, Diff), L),
        write(L), nl.


least_diff(X, Y, Diff) :-
        LD = [A,B,C,D,E,F,G,H,I,J], % declare the 10 variables
        domain(LD,0,9),
        all_different(LD),

        % the constraints:
        %   A+B+C+D+E = F+G+H+I+J
        X #= 10000*A+1000*B+100*C+10*D+E,
        Y #= 10000*F+1000*G+100*H+10*I+J,
        % positive difference 
        X #< Y,  
        % minimize the difference
        Diff #= Y - X,
        
        labeling([ff,bisect,down,minimize(Diff)],[X,Y]),
        labeling([ff],LD).

:- initialization(go).
