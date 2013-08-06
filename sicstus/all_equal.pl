/*

  Global constraint all_equal in SICStus Prolog.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_equal.html
  """
  Constraint
 
      all_equal(VARIABLES)
 
  Purpose
 
      Enforce all variables of the collection VARIABLES to take the same value.
 
  Example
      ‹(<5, 5, 5, 5>)
 
  The all_equal constraint holds since all its variables are fixed to value 5.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/all_equal_me.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 4,
        Lower = 0,
        Upper = 6,
        findall(X, all_equal_test(N,Lower, Upper,X), List),
        length(List,Len),
        write(List),nl,
        write(length:Len),nl,nl,
        fd_statistics.

all_equal_test(N, Lower, Upper, X) :-

        length(X,N),
        domain(X,Lower,Upper),
        
        % x = [5,5,5,5],
        all_equal(X),

        labeling([],X).
        


all_equal(X) :-
        ( fromto(X, [This,Next | Rest], [Next|Rest],[_]) do
              This #= Next
        ).
