/*

  Set covering in SICStus Prolog.

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  AMPL model: http://taha.ineg.uark.edu/setcover.txt


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering2.mzn
  * Comet   : http://www.hakank.org/comet/set_covering2.co
  * ECLiPSe : http://www.hakank.org/eclipse/set_covering2.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% First find the optimal value (MinVal) i.e. the number of telephones
% placed. Then find all the solutions with that value.
%
go :-
        write('Find the optimal solution'),nl,
        corners(N, Corners),
        set_covering2(N, Corners, MinVal,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(X, set_covering2(N, Corners,MinVal,X), L),
        length(L, Len),
        write(L),nl,
        format("It was ~d solutions\n", [Len]).


set_covering2(N, Corners, MinVal, Xs) :-

        matrix(Corners,[_NumStreets,2]),
        
        % where to place the telephone
        length(Xs,N),
        domain(Xs,0,1),
        
        % all streets must be covered
        ( foreach(Corner,Corners),
          param(Xs) 
        do
          % Xs[Corners[I,1]] + Xs[Corners[I,2]] #>= 1
          [From,To] = Corner,
          element(From,Xs,F),
          element(To,Xs,T),
          F + T #>= 1
        ),

        %
        % objective: minimize the number of telephones
        %
        sum(Xs, #=, Z),
        Z #= MinVal,

        %
        % either search for all solutions (for the minimum value) or
        % the optimal value
        %
        (
            ground(MinVal) 
        -> 
            labeling([], Xs)
        ;
            labeling([minimize(Z)], Xs)
        ),

        % write(z:Z),nl,
        write(x:Xs),nl,nl,
        fd_statistics.


% Suggested by Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



% corners of each street
%
% corners(NumberOfStreets, Corners)
%
corners(8, [[1,2],
            [2,3],
            [4,5],
            [7,8],
            [6,7],
            [2,6],
            [1,6],
            [4,7],
            [2,4],
            [5,8],
            [3,5]]).

