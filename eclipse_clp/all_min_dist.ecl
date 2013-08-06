/*

  Global constraint all_min_dist in ECLiPSe.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
  """
  Enforce for each pair (vari, varj) ‹ of distinct variables of the 
  collection VARIABLES that 
  |vari - varj| >= MINDIST.
  
  Example
   (2, <5, 1, 9, 3>)
  
  The all_min_dist constraint holds since the following expressions 
  |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
  to the first argument MINDIST = 2 of the all_min_dist constraint.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/all_min_dist.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/all_min_dist.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).




go :-
        N = 4,
        length(X,N),
        X :: 1..9,
        C :: 0..9,

        X = [5,1,9,3],
        all_min_dist(C, X),
        C #= 2,

        term_variables([X,C], Vars),
        search(Vars,0, first_fail, indomain_min, complete,[backtrack(Backtracks)]),
        
        writeln(x:X),
        writeln(c:C),
        writeln(backtracks:Backtracks),
        nl,
        fail.

% Note: If C is free
% all values from 0..(real value of C)
% will be generated.
all_min_dist(_,[]):- !.
all_min_dist(_,[_]):- !.
all_min_dist(C, [H|Ts]) :-
        ( foreach(T,Ts),
          param(C,H) do
              abs(H-T) #>= C
        ),
        all_min_dist(C,Ts).

