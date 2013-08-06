/*

  Discrete tomography in B-Prolog.

  Note: The origin of the problem is from ECLiPSe,
  but this model has been transformed in this way
     MiniZinc -> SICStus Prolog -> ECLiPSe -> B-Prolog
  Here is my own take at the problem.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        findall(_,discrete_tomography(1),_),
        findall(_,discrete_tomography(2),_),
        findall(_,discrete_tomography(3),_),
        findall(_,discrete_tomography(4),_).



discrete_tomography(P) :-

        problem(P,RowSums,ColSums),
        format('\nProblem ~d:\n',[P]),

        length(RowSums, Rows),
        length(ColSums, Cols),

        new_array(X, [Rows,Cols]),
        array_to_list(X, Vars),
        Vars :: 0..1,
        
        % check rows
        foreach(I in 1..Rows, 
                 RowSums[I] #= sum([ R : J in 1..Cols, [R], R @= X[I,J] ])),
        % writeln(here1),
        foreach(J in 1..Cols, 
                ColSums[J] #= sum([ C : I in 1..Rows, [C], C @= X[I,J] ])),

        labeling(Vars),
        print_square(X).


print_square(Square) :-
        Rows @= Square^length,
        Cols @= Square[1]^length,
        foreach(I in 1..Rows,
                (foreach(J in 1..Cols,
                         [S],
                         (
                             S @= Square[I,J],
                             (S =:= 0 ->write(' ') ; write('X'))
                         )
                        ),
                 nl
                )
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
