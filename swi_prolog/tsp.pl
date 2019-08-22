/*

  Traveling salesperson problem in SWI Prolog


  Inspired by the code from lecture notes
  Ulf Nilsson: Transparencies for the course TDDD08 Logic
  Programming, page 6f
  http://www.ida.liu.se/~TDDD08/misc/ulfni.slides/oh10.pdf

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Test all the simple instances.
%% The GLPK instance is much harder, see go2/0.
%%
go:-        
   time(once(do_tsp(nilsson))),  %% the original formulation
   time(once(do_tsp(nilsson2))), %% the generalized version
   time(once(do_tsp(chip))),
   time(once(do_tsp(ilog))),
   nl.


%%
%% Problem from GLPK:s example tsp.mod.
%% Compare with the MiniZinc model http://www.hakank.org/minizinc/tsp.mzn
%% and see more comments below.
%%
%% % 7,366,933,236 inferences, 477.695 CPU in 477.694 seconds (100% CPU, 15421831 Lips)
%% 
go2 :- 
        time(do_tsp(glpk)).


%%
%% Use a random cost matrix of size NxN with cost values of 1..MaxVal
%% and 0 in the diagonal.
%% 
%% Note: When using small MaxVal (say 10 or 20), the probability of a cost of 1
%% is quite high which is certainly the smallest cost for an edge and thus the
%% instance can be fairly fast even for larger problems, e.g. N = 150.
%%
%% For larger MaxVals (say 50) this probability of cost 1 is smaller
%% and this tends to increase the solving time, even for smaller N.
%%
%% Example runs:
%%
%% n=150
%% [mindist=1,maxDist=10]
%% cities=[11,3,25,19,2,7,20,4,12,9,34,28,15,29,1,10,5,17,13,23,8,14,30,6,45,21,22,26,40,36,24,49,32,37,27,57,43,52,54,33,16,38,39,72,74,59,42,44,48,18,31,41,60,66,64,58,47,35,53,50,65,55,77,70,82,88,69,78,75,85,68,76,62,63,51,67,84,46,87,56,83,79,93,117,81,61,114,89,115,102,110,101,92,71,86,111,123,91,73,99,80,94,106,108,104,100,113,118,96,90,95,116,119,105,98,135,127,97,141,107,124,112,120,133,109,136,126,131,146,145,150,140,129,148,144,122,121,134,130,149,139,128,125,147,137,132,138,142,103,143]
%% costs=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
%% path=[11,34,37,43,39,54,66,88,89,115,98,91,110,90,102,94,71,68,78,46,59,53,60,50,18,17,5,2,3,25,45,74,63,77,84,117,127,126,136,122,112,116,135,144,147,138,134,148,142,128,131,150,143,125,109,96,111,95,86,61,65,82,79,87,114,105,104,108,118,97,123,120,107,113,119,141,139,130,145,137,121,124,133,129,146,132,140,149,103,106,100,99,73,62,55,64,70,85,81,83,93,92,101,80,56,58,35,27,22,14,29,40,33,32,49,48,44,72,76,67,69,75,51,31,24,6,7,20,23,30,36,57,47,42,38,52,41,16,10,9,12,28,26,21,8,4,19,13,15,1]
%% cost=150
%%
%% % 319,777,982 inferences, 20.991 CPU in 20.992 seconds (100% CPU, 15233706 Lips)
%%
%% n=16
%% [mindist=9,maxDist=1998]
%% cities=[9,11,5,3,10,1,8,16,7,14,6,15,12,2,4,13]
%% costs=[182,168,60,72,804,387,406,248,448,13,24,9,78,363,51,18]
%% path=[9,7,8,16,13,12,15,4,3,5,10,14,2,11,6,1]
%% cost=3331
%% % 32,834,370 inferences, 2.140 CPU in 2.140 seconds (100% CPU, 15343003 Lips)
%
go3 :-
        N = 300,
        writeln(n=N),
        MaxVal = 10,
        generate_random_matrix_zero_diag(N, MaxVal,Matrix),
        (
         N =< 30
        ->
         writeln("Matrix:"),
         maplist(writeln,Matrix)
        ;
         true
        ),
        tsp(Matrix, Cities, Costs,Cost),
        writeln(cities=Cities),
        writeln(costs=Costs),
        writeln(cost=Cost),
        (
         N =< 30
        ->
         show_tour(Cities,Costs)
        ;
         circuit_path(Cities,Path),
         writeln(path=Path)
        ),
        nl.

%%
%% Wrapper
%%
do_tsp(P) :-
        format("Problem ~w~n", P),
        (
         P == nilsson
        -> 
         tsp_test(nilsson, Cities, Cost),
         writeln(cities=Cities),
         writeln(cost=Cost)
        ;
         tsp_test(P, Cities, Costs,Cost),
         writeln(cities=Cities),
         writeln(costs=Costs),
         writeln(cost=Cost),
         show_tour(Cities,Costs)
        ),
        nl.

%%
%% print the tour
%%
show_tour(Cities,Costs) :-
        circuit_path(Cities,Path),
        writeln(path=Path),
        length(Path,Len),
        Len1 #= Len-1,
        findall([I2,I3, C],
                (between(1,Len1,I),
                 element(I,Path,I2),
                 element(I2,Cities,I3),                 
                 element(I2,Costs,C)
                ),
                Tour1),
        element(1,Path,P1),
        element(1,Costs,C1),
        append([[1,P1,C1]],Tour1,Tour),
        maplist(format("Travel between ~w and ~w with cost ~w~n"),Tour),                 
        nl.

%%
%% TSP using a matrix, element/1 and circuit/1 constraints.
%%
%% This is a generalization of Nilsson's original version.
%%
tsp(Matrix, Cities, Costs,Cost) :-

        length(Matrix,Len),
        length(Cities,Len),
        Cities ins 1..Len,

        %% calculate upper and lower bounds of the Costs list
        flatten(Matrix,Dists1),
        subtract(Dists1,[0],Dists),
        
        min_list(Dists,MinDist),
        max_list(Dists,MaxDist),
        writeln([n=Len,mindist=MinDist,maxDist=MaxDist]),

        %% Costs ant total cost for the tour
        length(Costs,Len),
        Costs ins MinDist..MaxDist,
        sum(Costs,#=,Cost),
        Cost #> 0,

        %% all_different(Cities), %% implied constraint
        %% all_distinct(Cities), %% implied constraint        
        circuit(Cities),

        %% connect cities and costs 
        maplist(connect_cities_and_costs,Matrix,Cities,Costs),

        %% list_domains(Costs,CostDomains), %% show the inferred domains
        %% writeln(costDomains=CostDomains),

        flatten([Costs,Cities],Vars),
        %% flatten([Cities,Costs],Vars),        
        labeling([min(Cost)],Vars).

connect_cities_and_costs(Row,City,Cost) :-
        element(City,Row,Cost).



%%
%% Original formulation from Nilsson cited above.
%%
tsp_test(nilsson, Cities, Cost) :-
        Cities = [X1,X2,X3,X4,X5,X6,X7],
        element(X1,[ 0, 4, 8,10, 7,14,15],C1),
        element(X2,[ 4, 0, 7, 7,10,12, 5],C2),
        element(X3,[ 8, 7, 0, 4, 6, 8,10],C3),
        element(X4,[10, 7, 4, 0, 2, 5, 8],C4),
        element(X5,[ 7,10, 6, 2, 0, 6, 7],C5),
        element(X6,[14,12, 8, 5, 6, 0, 5],C6),
        element(X7,[15, 5,10, 8, 7, 5, 0],C7),
        Cost #= C1+C2+C3+C4+C5+C6+C7,
        circuit(Cities),
        labeling([min(Cost)], Cities).


%% 
%% This is a more general solution of the same
%% problem using for loops.
%%
%% It is somewhat most costly than the "explicit"
%% model above. It has the same number of 
%% backtracks, though.
%%
tsp_test(nilsson2,Cities, Costs,Cost) :-
        Matrix = 
        [[ 0, 4, 8,10, 7,14,15],
         [ 4, 0, 7, 7,10,12, 5],
         [ 8, 7, 0, 4, 6, 8,10],
         [10, 7, 4, 0, 2, 5, 8],
         [ 7,10, 6, 2, 0, 6, 7],
         [14,12, 8, 5, 6, 0, 5],
         [15, 5,10, 8, 7, 5, 0]],
        tsp(Matrix, Cities, Costs,Cost).


%%
%% This problem is from the SICStus example 
%% ./library/clpfd/examples/tsp.pl
%% The "chip" examples 
%%
tsp_test(chip,Cities, Costs,Cost) :-
        Matrix = 
        [[0,205,677,581,461,878,345],
         [205,0,882,427,390,1105,540],
         [677,882,0,619,316,201,470],
         [581,427,619,0,412,592,570],
         [461,390,316,412,0,517,190],
         [878,1105,201,592,517,0,691],
         [345,540,470,570,190,691,0]],
        tsp(Matrix, Cities, Costs,Cost).


%% This problem is from the SICStus example 
%% ./library/clpfd/examples/tsp.pl
%% The "ilog" examples
%%
tsp_test(ilog,Cities, Costs,Cost) :-
        Matrix = 
        [[2,4,4,1,9,2,4,4,1,9],
         [2,9,5,5,5,2,9,5,5,5],
         [1,5,2,3,3,1,5,2,3,3],
         [2,6,8,9,5,2,6,8,9,5],
         [3,7,1,6,4,3,7,1,6,4],
         [1,2,4,1,7,1,2,4,1,7],
         [3,5,2,7,6,3,5,2,7,6],
         [2,7,9,5,5,2,7,9,5,5],
         [3,9,7,3,4,3,9,7,3,4],
         [4,1,5,9,2,4,1,5,9,2]],
        tsp(Matrix, Cities, Costs,Cost).


%% This problem is from 
%% GLPK:s example tsp.mod
%% (via http://www.hakank.org/minizinc/tsp.mzn)
%% """
%% These data correspond to the symmetric instance ulysses16 from:
%% Reinelt, G.: TSPLIB - A travelling salesman problem library.
%% ORSA-Journal of the Computing 3 (1991) 376-84;
%% http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
%% 
%% The optimal solution is 6859
%% """
tsp_test(glpk,Cities, Costs,Cost) :-
        Matrix = 
        [[0,509,501,312,1019,736,656,60,1039,726,2314,479,448,479,619,150],
         [509,0,126,474,1526,1226,1133,532,1449,1122,2789,958,941,978,1127,542],
         [501,126,0,541,1516,1184,1084,536,1371,1045,2728,913,904,946,1115,499],
         [312,474,541,0,1157,980,919,271,1333,1029,2553,751,704,720,783,455],
         [1019,1526,1516,1157,0,478,583,996,858,855,1504,677,651,600,401,1033],
         [736,1226,1184,980,478,0,115,740,470,379,1581,271,289,261,308,687],
         [656,1133,1084,919,583,115,0,667,455,288,1661,177,216,207,343,592],
         [60,532,536,271,996,740,667,0,1066,759,2320,493,454,479,598,206],
         [1039,1449,1371,1333,858,470,455,1066,0,328,1387,591,650,656,776,933],
         [726,1122,1045,1029,855,379,288,759,328,0,1697,333,400,427,622,610],
         [2314,2789,2728,2553,1504,1581,1661,2320,1387,1697,0,1838,1868,1841,1789,2248],
         [479,958,913,751,677,271,177,493,591,333,1838,0,68,105,336,417],
         [448,941,904,704,651,289,216,454,650,400,1868,68,0,52,287,406],
         [479,978,946,720,600,261,207,479,656,427,1841,105,52,0,237,449],
         [619,1127,1115,783,401,308,343,598,776,622,1789,336,287,237,0,636],
         [150,542,499,455,1033,687,592,206,933,610,2248,417,406,449,636,0]
        ],
        tsp(Matrix, Cities, Costs,Cost).


%%
%% generate_random_matrix(N, MaxVal)
%%
%% Generate a random NxN matrix int the range of MinVal..MaxVal.
%%
generate_random_matrix(N,MinVal, MaxVal,Matrix) :-
        findall(Row,
                (between(1,N,_I),
                 findall(R,
                         (between(1,N,_J),
                          random_between(MinVal,MaxVal,R)),
                         Row
                        )
                ),
                Matrix).


%%
%% generate_random_matrix_zero_diag(N, MaxVal)
%%
%% Generate a random NxN matrix in the range of 1..MaxVal
%% except for the main diagonal where the value is 0.
%%
generate_random_matrix_zero_diag(N,MaxVal,Matrix) :-
        findall(Row,
                (between(1,N,I),
                 findall(R,
                         (
                         between(1,N,J),
                          (
                           I == J
                          ->
                           R = 0
                          ;
                           random_between(1,MaxVal,R)
                          )
                         ),
                         Row
                        )
                ),
                Matrix).