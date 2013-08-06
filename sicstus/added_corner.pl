/*

  Added corner puzzle in SICStus Prolog.

  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  """
  This puzzle requires that you enter the digits 1 through 8 in the circles and 
  squares (one digit in each figure) so that the number in each square is equal 
  to the sum on the numbers in the circles which  adjoin it.  
  ...
  
     C F C
     F   F
     C F C
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/added_corner.mzn
  * Comet   : http://www.hakank.org/comet/added_corner.co
  * ECLiPSe : http://www.hakank.org/eclipse/added_corner.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        X = [A,B,C,D,E,F,G,H],
        domain(X, 1,8),

        all_different(X),
        B #= A + C,
        D #= A + F,
        E #= C + H,
        G #= F + H,

        labeling([], X),

        format('~d ~d ~d\n', [A,B,C]),
        format('~d   ~d\n', [D,E]),
        format('~d ~d ~d\n', [F,G,H]),
        nl,
        fail.

