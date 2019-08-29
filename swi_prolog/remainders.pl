/*

  Remainder problem in SWI Prolog

  """
  11.  Is there a number which when divided by 3 gives a remainder of 1;
  when divided by 4, gives a remainder of 2; when divided by 5, gives a
  remainder of 3; and when divided by 6, gives a remainder of 4?
  (Kordemsky)
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :- findall(X,solve(X,10000),L),writeln(L).

solve(X,Max) :-
        [X,A,B,C,D] ins 1..Max,
        X #> 0,
        X #= A*3 + 1,
        X #= B*4 + 2,
        X #= C*5 + 3,
        X #= D*6 + 4,
        labeling([],[X,A,B,C,D]).

