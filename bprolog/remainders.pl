/*

  Remainder problem in B-Prolog.

  """
  11.  Is there a number which when divided by 3 gives a remainder of 1;
  when divided by 4, gives a remainder of 2; when divided by 5, gives a
  remainder of 3; and when divided by 6, gives a remainder of 4?
  (Kordemsky)
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :- findall(X,solve(X,10000),L),write(L),nl.

solve(X,Max) :-
        [X,A,B,C,D] :: 1..Max,
        X #> 0,
        X #= A*3 + 1,
        X #= B*4 + 2,
        X #= C*5 + 3,
        X #= D*6 + 4,
        labeling([],[X,A,B,C,D]).

