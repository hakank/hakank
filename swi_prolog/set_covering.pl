/*

  Set covering in SWI Prolog

  Placing of firestations, from Winston "Operations Research", page 486

  Solution:
  
    z=2
    x=[0,1,0,1,0,0]

  I.e. place the fire stations in cities 2 and 4.

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        problem(1, MinDistance, Distance),
        
        writeln(min_distance=MinDistance),
        
        % distance between the cities
        length(Distance,NumCities),
        writeln(num_cities=NumCities),

        % where to place the fire stations: 1 if placed in this city.
        length(X,NumCities),
        X ins 0..1,

        %% For each city, find the indices of the nearby cities.
        findall(Is,
                (member(City,Distance),
                 findall(I,(
                            between(1,NumCities,I),
                            element(I,City,D),D #=< MinDistance),
                         Is)
                ),
                CitiesCovered
               ),
        % Ensure that all cities has at least one fire station.
        maplist(sum_nearby_cities(X),CitiesCovered),
        
        sum(X,#=,Z),

        labeling([ff,min(Z)], X),
        writeln(z=Z),
        writeln(x=X).

% Ensure that the nearby cities has at least one fire station
sum_nearby_cities(X,Is) :-
        extract_from_indices(Is,X,Xs),
        sum(Xs,#>=,1).

%
% data
%
%
% Placing of firestations, from Winston "Operations Research", page 486
%
problem(1, MinDistance, Distance) :-
        MinDistance = 15, % minimum distance 
        Distance    = [[0,10,20,30,30,20],  % distances between the cities
                       [10,0,25,35,20,10],
                       [20,25,0,15,30,20],
                       [30,35,15,0,15,25],
                       [30,20,30,15,0,14],
                       [20,10,20,25,14,0]].
