/*

  (Decomposition of) global constraint nvalue in B-Prolog.

  From MiniZinc:
  """
  Requires that the number of distinct values in 'x' is 'n'.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        Len = 5,
        length(X,Len),
        X :: 1..Len,
        N :: 1..Len,      

        nvalue(N,X),

        term_variables([X,N], Vars),

        labeling(Vars),

        writeln([n:N, x:X]),
        fail.


%
% nvalue(?N,?X)
%
% Requires that the number of distinct values in X is N.
%
nvalue(N, X) :-
        length(X,Len),
        N #= sum([ (sum([ (X[J] #= I) : J in 1..Len]) #> 0) : I in 1..Len]).
