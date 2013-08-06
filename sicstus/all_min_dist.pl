/*

  Global constraint all_min_dist in SICStus Prolog.

  
  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
  """
  Enforce for each pair (vari, varj)â€‹ of distinct variables of the 
  collection VARIABLES that 
  |vari - varj| >= MINDIST.
  
  Example
   (2, <5, 1, 9, 3>)
  
  The all_min_dist constraint holds since the following expressions 
  |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
  to the first argument MINDIST = 2 of the all_min_dist constraint.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/all_min_dist.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 4,
        length(X,N),
        domain(X,1,9),
        C in 0..9,

        % X = [5,1,9,3],
        all_min_dist(C, X),
        C #= 2,

        append(X,[C], Vars),
        labeling([ffc,bisect,up],Vars),

        write(x:X),nl,
        write(c:C),nl,
        % fail,
        fd_statistics.


all_min_dist(_,[]):- !.
all_min_dist(_,[_]):- !.
all_min_dist(C, [H|Ts]) :-
        ( foreach(T,Ts),
          param(C,H) do
              abs(H-T) #>= C
        ),
        all_min_dist(C,Ts).

