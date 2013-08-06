/*

  Futoshiki problem in SICStus Prolog.

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
 

  Model inspired from the Minion/Tailor example futoshiki.eprime .

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/futoshiki.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        findall(_,futoshiki(1),_),
        findall(_,futoshiki(2),_),
        findall(_,futoshiki(3),_).

futoshiki(P) :-
        
        format('\nProblem ~w\n',[P]),
        problem(P,Problem,LessThans),

        % variables and domains
        matrix(Problem,[N,N]),
        matrix(X,[N,N]),
        append(X,XList),
        domain(XList,1,N),

        % initial data
        ( foreach(XRow,X),
          foreach(PRow,Problem) do
              ( foreach(XX,XRow),
                foreach(PP,PRow) do
                    PP > 0 -> 
                    XX #= PP 
              ;
                    true
                    )
        ),

        %
        % constraints
        %        
        latin_square(X),

        ( foreach([I1,J1,I2,J2],LessThans),
          param(X) do
              matrix_element(X,I1,J1,X1),
              matrix_element(X,I2,J2,X2),
              X1 #< X2
        ),

        % search
        labeling([], XList),

        % output
        ( foreach(Row,X) do
              write(Row),nl
        ),

        fd_statistics.


latin_square(X) :-
        ( foreach(Row,X) do
              all_distinct(Row)
        ),
        transpose(X,XTransposed),
        ( foreach(Column,XTransposed) do
              all_distinct(Column)
        ).


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


% From http://www.sudoku-puzzles.net/futoshiki_6x6.html
problem(3,
        [[5,0,2,0,1,0],
         [6,0,1,0,0,0],
         [0,0,3,0,0,0],
         [0,0,4,0,0,0],
         [0,0,0,4,2,5],
         [0,0,0,1,3,0]],
        
        [[1,6, 2,6],
         [4,1, 3,1]]).
         
         
         

         
        
