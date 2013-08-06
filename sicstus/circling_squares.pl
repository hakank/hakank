/*

  Circling the squares problem in SICStus Prolog.

 From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/#circling
  """
  Circling the Squares from "Amusements in Mathematics, Dudeney",
  number 43.

  The puzzle is to place a different number in each of the ten squares
  so that the sum of the squares of any two adjacent numbers shall be
  equal to the sum of the squares of the two numbers diametrically
  opposite to them. The four numbers placed, as examples, must stand as
  they are. Fractions are not allowed, and no number need contain more
  than two figures. 
 
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/circling_squares.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/circling_squares.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        findall(LD, circling_squares(LD),_).


write_list([A,B,C,D,E,F,G,H,I,K]) :-
        format("     ~d  ~d\n", [K,A]),
        format("   ~d       ~d\n", [I,B]),
        format(" ~d           ~d\n", [H,C]),
        format("   ~d       ~d\n", [G,D]),
        format("     ~d  ~d\n", [F,E]),
        nl,nl.
   


circling_squares(LD) :-
        LD = [A,B,C,D,E,F,G,H,I,K],
        domain(LD, 1, 99),
        all_different(LD),

        A #= 16,
        B #= 2,
        F #= 8,
        G #= 14,

        s(A,B,  F,G),
        s(B,C,  G,H),
        s(C,D,  H,I),
        s(D,E,  I,K),
        s(E,F,  K,A),
        
        labeling([],LD),
        write_list(LD).


%
% help predicate
%
s(X1,X2, Y1, Y2 ) :-
        X1*X1 + X2*X2  #= Y1*Y1 + Y2*Y2.

