/*

  Set covering in SICStus Prolog.

  Placing of firestations, from Winston "Operations Research", page 486

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/set_covering.ecl
  * Comet   : http://www.hakank.org/comet/set_covering.co
  * Gecode  : http://www.hakank.org/gecode/set_covering.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).




go :-
        problem(1, MinDistance, Distance),

        write(min_distance:MinDistance),nl,

        % distance between the cities
        matrix(Distance, [NumCities,NumCities]),

        write(num_cities:NumCities),nl,

        % where to place the fire stations: 1 if placed in this city,
        % 0 else
        length(Xs,NumCities),
        domain(Xs,0,1),

        % calculate the number of covered fire stations
        ( foreach(City,Distance),
          param(MinDistance,Xs) do
              ( foreach(C,City),
                foreach(X,Xs),
                fromto(0,In,Out,Sum),
                param(MinDistance) do
                    Z in 0..1,
                    % is this city within the minimum distance?
                    C #=< MinDistance #<=> Z #= 1,
                    Out #= In + Z*X
              ),
              Sum #>= 1
        ),

        % objective: minimize the number of fire stations
        sum(Xs,#=,NumFirestations),

        labeling([minimize(NumFirestations)],Xs),
        write(num_firestations:NumFirestations),nl,
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



% Placing of firestations, from Winston "Operations Research", page 486
%
% See http://www.hakank.org/minizinc/set_covering.mzn
problem(1,
        15, % minimum distance 
        [[0,10,20,30,30,20],  % distances between the cities
         [10,0,25,35,20,10],
         [20,25,0,15,30,20],
         [30,35,15,0,15,25],
         [30,20,30,15,0,14],
         [20,10,20,25,14,0]]).
