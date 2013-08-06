/*

  Added corner problem in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        findall(X, added_corner(X), L),
        length(L, Len),
        format("It was ~d solutions.\n", [Len]).


added_corner(X) :-
        Digits = 1..8,

        X = [A,B,C,D,E,F,G,H],
        X :: Digits,

        alldifferent(X),
        B #= A + C,
        D #= A + F,
        E #= C + H,
        G #= F + H,

        labeling(X),

        format("~d ~d ~d\n", [A,B,C]),
        format("~d   ~d\n",  [D,E]),
        format("~d ~d ~d\n", [F,G,H]),
        nl.

        
