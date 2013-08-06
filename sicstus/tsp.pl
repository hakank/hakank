/*

  Traveling salesperson problem using loops in SICStus Prolog.

  Inspired by the code from lecture notes
  Ulf Nilsson: Transparencies for the course TDDD08 Logic
  Programming, page 6f
  http://www.ida.liu.se/~TDDD08/misc/oh10.pdf

  However this version uses loops.

  

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go:-        
        do_tsp(nilsson),  % the original formulation
        do_tsp(nilsson2), % the loop version
        do_tsp(chip),
        do_tsp(ilog).


%
% Problem from GLPK:s example tsp.mod.
% Compare with the MiniZinc model http://www.hakank.org/minizinc/tsp.mzn
% and see more comments below.
%
% cities:[14,4,2,8,11,15,6,1,10,16,9,7,12,13,5,3]
% cost:6859
% 
% Resumptions: 1474711
% Entailments: 133242
% Prunings: 2328166
% Backtracks: 73286
% Constraints created: 621
% 
% Time: 6.6s
% 
% (GLPK solves tsp.mod in 9.2s using the following call:
%  glpsol -m tsp.mod
% )
%
go2 :- 
        costs(Costs),
        edges(Edges),
        N = 16,
        edges_to_matrix(N,Edges,Costs,Matrix),
        print_matrix(Matrix),
        tsp(Matrix, Cities, Cost),
        write(cities:Cities), nl,
        write(cost:Cost), nl,nl,
        fd_statistics.        

%
% convert edge representation to a matrix
%
edges_to_matrix(N,Edges,Costs,Matrix) :-
        maximum(MaxCost,Costs),
        matrix(Matrix,[N,N]),
        % append(Matrix,MatrixList),
        % domain(MatrixList,0,MaxCost),
        ( foreach([From,To], Edges),
          count(I,1,_),
          param(Matrix,Costs) do
              element(I,Costs,Cost),
              matrix_element(Matrix,From,To,Cost)
        ),
        % zero the diagonal
        ( for(I,1,N),
          param(Matrix) do
              matrix_element(Matrix,I,I,0)
        ).

print_matrix(Matrix) :- ( foreach(Row,Matrix) do write(Row),nl),nl.
        

do_tsp(P) :-
        format("Problem ~s", P),nl,
        tsp_test(P, Cities, Cost),
        write(cities:Cities), nl,
        write(cost:Cost), nl,
        fd_statistics,nl.


%
% TSP using a matrix. 
%
% This is a generalized of Nilsson's original version.
%
tsp(Matrix, Cities, Cost) :-
        length(Costs, Len),
        length(Cities, Len),
        domain(Cities, 1, Len),
        (
            foreach(Row, Matrix),
            foreach(X,Cities),
            foreach(C, Costs)
        do
            element(X,Row,C)
        ),
        sum(Costs,#=,Cost),
        circuit(Cities),

        % this labeling is better for the GLPK problem in go2/0.
        labeling([min,step,up,minimize(Cost)], Cities).
        % this labeling is better for the problems in go/0. 
        % labeling([minimize(Cost)], Cities).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


%
% Original formulation from Nilsson.
% fd_statistics:
% Resumptions: 384
% Entailments: 100
% Prunings: 476
% Backtracks: 13
% Constraints created: 11
%
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
        labeling([minimize(Cost)], Cities).


% 
% This is a more general solution of the same
% problem using for loops.
%
% It is somewhat most costly than the "explicit"
% model above. It has the same number of 
% backtracks, though.
%
% fd_statistics:
% Resumptions: 400
% Entailments: 108
% Prunings: 478
% Backtracks: 14
% Constraints created: 32
%
tsp_test(nilsson2,Cities, Cost) :-
        Matrix = [[ 0, 4, 8,10, 7,14,15],
                  [ 4, 0, 7, 7,10,12, 5],
                  [ 8, 7, 0, 4, 6, 8,10],
                  [10, 7, 4, 0, 2, 5, 8],
                  [ 7,10, 6, 2, 0, 6, 7],
                  [14,12, 8, 5, 6, 0, 5],
                  [15, 5,10, 8, 7, 5, 0]],
        tsp(Matrix, Cities, Cost).


%
% This problem is from the SICStus example 
% ./library/clpfd/examples/tsp.pl
% The "chip" examples 
%
% fd_statistics: 
% Resumptions: 620
% Entailments: 164
% Prunings: 708
% Backtracks: 20
% Constraints created: 32
% 

tsp_test(chip,Cities, Cost) :-
        Matrix = [[0,205,677,581,461,878,345],
                  [205,0,882,427,390,1105,540],
                  [677,882,0,619,316,201,470],
                  [581,427,619,0,412,592,570],
                  [461,390,316,412,0,517,190],
                  [878,1105,201,592,517,0,691],
                  [345,540,470,570,190,691,0]],
        tsp(Matrix, Cities, Cost).


% This problem is from the SICStus example 
% ./library/clpfd/examples/tsp.pl
% The "ilog" examples
%
% fd_statistics:
% Resumptions: 5960
% Entailments: 1557
% Prunings: 8108
% Backtracks: 292
% Constraints created: 59
%
tsp_test(ilog,Cities, Cost) :-
        Matrix = [[2,4,4,1,9,2,4,4,1,9],
                  [2,9,5,5,5,2,9,5,5,5],
                  [1,5,2,3,3,1,5,2,3,3],
                  [2,6,8,9,5,2,6,8,9,5],
                  [3,7,1,6,4,3,7,1,6,4],
                  [1,2,4,1,7,1,2,4,1,7],
                  [3,5,2,7,6,3,5,2,7,6],
                  [2,7,9,5,5,2,7,9,5,5],
                  [3,9,7,3,4,3,9,7,3,4],
                  [4,1,5,9,2,4,1,5,9,2]],
        tsp(Matrix, Cities, Cost).


% This problem is from 
% GLPK:s example tsp.mod
% (via http://www.hakank.org/minizinc/tsp.mzn)
% """
% These data correspond to the symmetric instance ulysses16 from:
% Reinelt, G.: TSPLIB - A travelling salesman problem library.
% ORSA-Journal of the Computing 3 (1991) 376-84;
% http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
% 
% The optimal solution is 6859
% """
costs([
509,501,312,1019,736,656,60,1039,726,2314,479,448,479,619,150,509,126,474,1526,
1226,1133,532,1449,1122,2789,958,941,978,1127,542,501,126,541,1516,1184,1084,
536,1371,1045,2728,913,904,946,1115,499,312,474,541,1157,980,919,271,
1333,1029,2553,751,704,720,783,455,1019,1526,1516,1157,478,583,996,858,855,
1504,677,651,600,401,1033,736,1226,1184,980,478,115,740,470,379,1581,
271,289,261,308,687,656,1133,1084,919,583,115,667,455,288,1661,177,216,207,
343,592,60,532,536,271,996,740,667,1066,759,2320,493,454,479,598,206,
1039,1449,1371,1333,858,470,455,1066,328,1387,591,650,656,776,933,726,
1122,1045,1029,855,379,288,759,328,1697,333,400,427,622,610,2314,2789,
2728,2553,1504,1581,1661,2320,1387,1697,1838,1868,1841,1789,2248,479,
958,913,751,677,271,177,493,591,333,1838,68,105,336,417,448,941,904,
704,651,289,216,454,650,400,1868,68,52,287,406,479,978,946,720,600,261,207,479,
656,427,1841,105,52,237,449,619,1127,1115,783,401,308,343,598,776,622,
1789,336,287,237,636,150,542,499,455,1033,687,592,206,933,610,2248,417,
406,449,636]).

edges([
[1,2],[1,3],[1,4],[1,5],[1,6],[1,7],[1,8],[1,9],[1,10],[1,11],[1,12],
[1,13],[1,14],[1,15],[1,16],
[2,1],[2,3],[2,4],[2,5],[2,6],[2,7],[2,8],[2,9],[2,10],[2,11],[2,12],
[2,13],[2,14],[2,15],[2,16],
[3,1],[3,2],[3,4],[3,5],[3,6],[3,7],[3,8],[3,9],[3,10],[3,11],[3,12],
[3,13],[3,14],[3,15],[3,16],
[4,1],[4,2],[4,3],[4,5],[4,6],[4,7],[4,8],[4,9],[4,10],[4,11],[4,12],
[4,13],[4,14],[4,15],[4,16],
[5,1],[5,2],[5,3],[5,4],[5,6],[5,7],[5,8],[5,9],[5,10],[5,11],[5,12],
[5,13],[5,14],[5,15],[5,16],
[6,1],[6,2],[6,3],[6,4],[6,5],[6,7],[6,8],[6,9],[6,10],[6,11],[6,12],
[6,13],[6,14],[6,15],[6,16],
[7,1],[7,2],[7,3],[7,4],[7,5],[7,6],[7,8],[7,9],[7,10],[7,11],
[7,12],[7,13],[7,14],[7,15],[7,16],
[8,1],[8,2],[8,3],[8,4],[8,5],[8,6],[8,7],[8,9],[8,10],[8,11],[8,12],
[8,13],[8,14],[8,15],[8,16],
[9,1],[9,2],[9,3],[9,4],[9,5],[9,6],[9,7],[9,8],[9,10],[9,11],[9,12],
[9,13],[9,14],[9,15],[9,16],
[10,1],[10,2],[10,3],[10,4],[10,5],[10,6],[10,7],[10,8],[10,9],[10,11],
[10,12],[10,13],[10,14],[10,15],[10,16],
[11,1],[11,2],[11,3],[11,4],[11,5],[11,6],[11,7],[11,8],[11,9],[11,10],
[11,12],[11,13],[11,14],[11,15],[11,16],
[12,1],[12,2],[12,3],[12,4],[12,5],[12,6],[12,7],[12,8],[12,9],[12,10],
[12,11],[12,13],[12,14],[12,15],[12,16],
[13,1],[13,2],[13,3],[13,4],[13,5],[13,6],[13,7],[13,8],[13,9],[13,10],
[13,11],[13,12],[13,14],[13,15],[13,16],
[14,1],[14,2],[14,3],[14,4],[14,5],[14,6],[14,7],[14,8],[14,9],[14,10],
[14,11],[14,12],[14,13],[14,15],[14,16],
[15,1],[15,2],[15,3],[15,4],[15,5],[15,6],[15,7],[15,8],[15,9],[15,10],
[15,11],[15,12],[15,13],[15,14],[15,16],
[16,1],[16,2],[16,3],[16,4],[16,5],[16,6],[16,7],[16,8],[16,9],[16,10],
[16,11],[16,12],[16,13],[16,14],[16,15]]).
