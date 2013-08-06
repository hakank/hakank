/*

  Futoshiki problem in B-Prolog.

  http://en.wikipedia.org/wiki/Futoshiki
  """
  The puzzle is played on a square grid, such as 5 x 5. The objective
  is to place the numbers 1 to 5 (or whatever the dimensions are) such 
  that each row, and column contains each of the digits 1 to 5. Some 
  digits may be given at the start. In addition, inequality
  constraints are also initially specifed between some of the squares, 
  such that one must be higher or lower than its neighbour. These 
  constraints must be honoured as the grid is filled out.
  """
  http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley
 



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% Reporting both time and backtracks
%
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).



go :-
        time2(findall(_,futoshiki(1),_)),
        time2(findall(_,futoshiki(2),_)),
        time2(findall(_,futoshiki(3),_)).

futoshiki(P) :-
        
        format("\nProblem ~w\n",[P]),
        problem(P,Problem,LessThans),

        N @= Problem^length,
        new_array(X,[N,N]),
        array_to_list(X,XVar),
        XVar :: 1..N,

        % Fill the data
        foreach(I in 1..N, J in 1..N,
                ( Problem[I,J] > 0 -> X[I,J] #= Problem[I,J];true)
               ),

        % constraints
        latin_square(X),
        foreach([I1,J1,I2,J2] in LessThans,
                [X1,X2],
                (
                    X1 @= X[I1,J1],
                    X2 @= X[I2,J2],
                    X1 #< X2
                )
        ),

        % search
        labeling(XVar),

        % output
        Rows @= X^rows,
        foreach(Row in Rows, writeln(Row)),
        nl.


latin_square(X) :-
        Rows @= X^rows,
        foreach(Row in Rows, alldifferent(Row)),
        Columns @= X^columns,
        foreach(Column in Columns, alldifferent(Column)).


% Example from Tailor model futoshiki.param/futoshiki.param
%
% Solution:
% 5 1 3 2 4
% 1 4 2 5 3
% 2 3 1 4 5
% 3 5 4 1 2
% 4 2 5 3 1
% 
% Futoshiki instance, by Andras Salamon
%
problem(1, 
        [[0,0,3,2,0],  % problem grid
         [0,0,0,0,0],
         [0,0,0,0,0],
         [0,0,0,0,0],
         [0,0,0,0,0]],

        [[1,2,1,1], % [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
         [1,4,1,5],
         [2,3,1,3],
         [3,3,2,3],
         [3,4,2,4],
         [2,5,3,5],
         [3,2,4,2],
         [4,4,4,3],
         [5,2,5,1],
         [5,4,5,3],
         [5,5,4,5]]).


% Example from http://en.wikipedia.org/wiki/Futoshiki
% Solution:
% 5 4 3 2 1
% 4 3 1 5 2
% 2 1 4 3 5
% 3 5 2 1 4
% 1 2 5 4 3
%
problem(2,
        [[0,0,0,0,0],
         [4,0,0,0,2],
         [0,0,4,0,0],
         [0,0,0,0,4],
         [0,0,0,0,0]],

        [[1,2, 1,1],
         [1,4, 1,3],
         [1,5, 1,4],
         [4,4, 4,5],
         [5,1, 5,2],
         [5,2, 5,3]]).


% From http://www.sudoku-puzzles.net/futoshiki06x6.html
problem(3,
        [[5,0,2,0,1,0],
         [6,0,1,0,0,0],
         [0,0,3,0,0,0],
         [0,0,4,0,0,0],
         [0,0,0,4,2,5],
         [0,0,0,1,3,0]],
        
        [[1,6, 2,6],
         [4,1, 3,1]]).
