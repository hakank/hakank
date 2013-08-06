/*

  Set covering in ECLiPSe.

  Placing of firestations, from Winston "Operations Research", page 486

  Compare with the MiniZinc model 
  http://www.hakank.org/minizinc/set_covering.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(propia).


go :-
        problem(1, MinDistance, Distance),

        writeln(min_distance:MinDistance),

        % distance between the cities
        dim(Distance, [NumCities,NumCities]),

        writeln(num_cities:NumCities),

        % where to place the fire stations: 1 if placed in this city
        dim(X,[NumCities]),
        X :: 0..1,


        %
        % calculate the number of covered fire stations
        %
        ( for(I,1,NumCities),
          param(Distance,NumCities,MinDistance,X) do
              ( for(J,1,NumCities),
                fromto(0,In,Out,Sum),
                param(Distance,MinDistance,I,X) do
                    Out #= In + X[J]*(Distance[I,J] #=< MinDistance)
              ),
              Sum #>= 1
        ),

        %
        % objective: minimize the number of fire stations
        %
        flatten_array(X, Vars),
        Z #= sum(Vars),

        minimize(search(Vars,0,first_fail,indomain,complete,[backtrack(Backtracks)]),Z),
        writeln(z:Z),
        writeln(x:X),
        writeln(backtracks:Backtracks).


%
% data
%

% Placing of firestations, from Winston "Operations Research", page 486
%
% See http://www.hakank.org/minizinc/set_covering.mzn
%
problem(1,
        15, % minimum distance 
        []([](0,10,20,30,30,20),  % distances between the cities
           [](10,0,25,35,20,10),
           [](20,25,0,15,30,20),
           [](30,35,15,0,15,25),
           [](30,20,30,15,0,14),
           [](20,10,20,25,14,0))).
