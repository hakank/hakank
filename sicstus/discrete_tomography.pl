/*

  Discrete tomography in SICStus Prolog.

  Problem from http://eclipse-clp.org/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.
 
  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:
 
  ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0                         
  0                         
  8      * * * * * * * *    
  2      *             *    
  6      *   * * * *   *    
  4      *   *     *   *    
  5      *   *   * *   *    
  3      *   *         *    
  7      *   * * * * * *    
  0                         
  0                         
 
 
  Eclipse solution by Joachim Schimpf, IC-Parc
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/tomography.mzn
  * Comet: http://www.hakank.org/comet/discrete_tomography.co
  * Tailor/Essence': http://www.hakank.org/tailor/discrete_tomography.eprime
  * Gecode: http://www.hakank.org/gecode/discrete_tomography.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


% Note: This is about the same as the ECLiPSe model cited above, except
%       that it don't use matrices, slices etc.


go :-
        findall(_,discrete_tomography(1),_),
        findall(_,discrete_tomography(2),_),
        findall(_,discrete_tomography(3),_),
        findall(_,discrete_tomography(4),_).



discrete_tomography(P) :-

        problem(P,RowSums,ColSums),
        format('Problem ~d',P),nl,

        length(RowSums, Rows),
        length(ColSums, Cols),

        matrix(X, [Rows,Cols]),
        % length(X, Rows),
        transpose(X,XTransposed),
        % length(XTransposed, Columns),

        append(X, Vars),
        domain(Vars, 0, 1),
        
        % check rows
        (
            foreach(Row, X),
            foreach(RSum, RowSums)
        do
            sum(Row, #=, RSum)
        ),

        % check columns
        (
            foreach(Column, XTransposed),
            foreach(CSum, ColSums)
        do
            sum(Column, #=, CSum)
        ),

        labeling([], Vars),

        pretty_print(X),
        fd_statistics.

matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


pretty_print(Matrix) :-
        nl,
        ( foreach(Row, Matrix) do
            ( foreach(R, Row) do
                  R #= 0 -> write(' ')
            ;
                            write('*')
            ),
            nl
        ),
        nl.



%
% The three first problems are from the ECLiPSe model:
%
% The above stated problem
problem(1, 
        [0,0,8,2,6,4,5,3,7,0,0], % row sums
        [0,0,7,1,6,3,4,5,2,7,0,0]). % column sums


problem(2,
        [10,4,8,5,6],
        [5,3,4,0,5,0,5,2,2,0,1,5,1]).


%
% This give three slightly different solutions.
problem(3,
        [11,5,4],
        [3,2,3,1,1,1,1,2,3,2,1]).


% This is my own problem.
problem(4,
        [0,2,2,2,2,2,8,8,4,4,4,4,4,0],
        [0,0,0,12,12,2,2,2,2,7,7,0,0,0]).
