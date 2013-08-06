/*

  Global constraint all_equal in B-Prolog.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_equal.html
  """
  Constraint
 
      all_equal(VARIABLES)
 
  Purpose
 
      Enforce all variables of the collection VARIABLES to take the same value.
 
  Example
      (<5, 5, 5, 5>)
 
  The all_equal constraint holds since all its variables are fixed to value 5.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 4,
        Lower = 0,
        Upper = 6,
        findall(X, all_equal_test(N,Lower, Upper,X), List),
        length(List,Len),
        writeln(List),
        writeln(length:Len),nl.

all_equal_test(N, Lower, Upper, X) :-
        length(X,N),
        X :: Lower..Upper,
        % x = [5,5,5,5],
        all_equal(X),
        labeling([],X).


all_equal(X) :-
        foreach(I in 2..X^length,X[I-1] #= X[I]).
