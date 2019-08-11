/*

  Langford's number problem L(2,N) in SWI Prolog

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


  Note: For k=4 there are two different solutions:
     solution:[4,1,3,1,2,4,3,2]
     position:[2,5,3,1,4,8,7,6]
  and
     solution:[2,3,4,2,1,3,1,4]
     position:[5,1,2,3,7,4,6,8]

  With this symmetry breaking

     Solution[1] < Solution[K2],

  then just the second solution is shown.

  Note: There are only solutions when K mod 4 == 0 or K mod 4 == 3.
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Solution of K=4 (see above).
%%
go :-
    K = 4,
    format("K: ~d~n", K),
    time(langford(K, Solution, Position)),
    format("Solution: ~w~n", [Solution]),
    format("Position: ~w~n", [Position]),
    nl.

%%
%% Get the first (if any) solutions for K in 2..40
%%
go2 :- 
        between(2,60,K),
        format("K: ~d\n", [K]),
        ( time(langford(K, Solution, _Position))
        -> 
          (
           nonvar(Solution)
          -> 
           format("Solution: ~w ~n", [Solution])
          ;
            writeln("No solution")
         )
        ;
          writeln("")
        ),
        nl,
        fail,
        nl.

go2.


%%
%% Count the number of solutions
%%
go3 :- 
        between(2,11,K),
        format("k: ~d~n",[K]),        
        time(findall(1, langford(K, _Solution, _Position), L)),
        length(L,Len),
        format("K: ~d = ~d solutions~n~n", [K, Len]),
        fail,
        nl.

go3.

 
langford(K, Solution, Position) :-

        ( \+ ((K mod 4 #= 0; K mod 4 #= 3))
        -> 
          writeln("There is no solution for K unless K mod 4 == 0 or K mod 4 == 3"),
          fail
        ;
          true
        ),
        
        K2 #= 2*K,
        length(Position,K2),
        Position ins 1..K2,
        
        length(Solution,K2),
        Solution ins 1..K,
        
        all_distinct(Position),

        % symmetry breaking:
        element(1,Solution,Solution1),
        element(K2,Solution,SolutionK2),
        Solution1 #< SolutionK2,

        % main constraints
        numlist(1,K,Is),
        maplist(langford_constraints2(K,Position,Solution),Is),
        
        append(Solution,Position,Vars),
        labeling([ff,enum], Vars).

%% for maplist
langford_constraints2(K,Position,Solution,I) :-
        KI #= K+I,
        element(KI, Position,PositionKI),
        element(I, Position,PositionI),
        I1 #= I+1,
        PositionKI #= PositionI + I1,
        element(I,Position,PositionI),
        element(PositionI,Solution,I), 
        element(PositionKI,Solution,I).

