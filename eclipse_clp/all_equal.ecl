/*

  Global constraint all_equal in ECLiPSe.

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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/all_equal_me.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/all_equal.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).


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
        
        % X = [5,5,5,5],
        all_equal(X),

        labeling(X).
        


all_equal(X) :-
        ( fromto(X, [This,Next | Rest], [Next|Rest],[_]) do
              This #= Next
        ).
