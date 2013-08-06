/*

  Global constraint all_min_dist in B-Prolog.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
  """
  Enforce for each pair (vari, varj) of distinct variables of the 
  collection VARIABLES that 
  |vari - varj| >= MINDIST.
  
  Example
   (2, <5, 1, 9, 3>)
  
  The all_min_dist constraint holds since the following expressions 
  |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
  to the first argument MINDIST = 2 of the all_min_dist constraint.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 4,
        length(X,N),
        X :: 1..9,
        C :: 0..9,

        % X = [5,1,9,3],
        C #= 2,

        findall([C,X],
                (
                    all_min_dist(C, X),
                    term_variables([X,C], Vars),
                    labeling([ffc],Vars)
                ),
                L),
        length(L,Len),
        foreach([C,X] in L,
                (
                write(x:X),nl,
                write(c:C),nl
                )
               ),
        writeln(len:Len).



all_min_dist(_,[]):- !.
all_min_dist(_,[_]):- !.
all_min_dist(C, [H|Ts]) :-
        foreach(T in Ts,
              abs(H-T) #>= C
        ),
        all_min_dist(C,Ts).

