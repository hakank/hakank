/*

  Remainder problem in SICStus Prolog.

  """
  11.  Is there a number which when divided by 3 gives a remainder of 1;
  when divided by 4, gives a remainder of 2; when divided by 5, gives a
  remainder of 3; and when divided by 6, gives a remainder of 4?
  (Kordemsky)
  """

  Compare with the following model:
  * ECLiPSe: http://www.hakank.org/eclipse/remainders.ecl 

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :- findall(X,remainder(X,10000),L),write(L),nl.

remainder(X,Max) :-
        domain([X,A,B,C,D],1,Max),
        X #> 0,
        X #= A*3 + 1,
        X #= B*4 + 2,
        X #= C*5 + 3,
        X #= D*6 + 4,
        labeling([],[X,A,B,C,D]).

