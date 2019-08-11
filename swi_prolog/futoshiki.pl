/*

  Futoshiki puzzle in SWI Prolog

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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        between(1,3,P),
        findall(_,futoshiki(P),_L),
        nl,
        fail,
        nl.

go.

        

futoshiki(P) :-
   
   format("\nProblem ~d\n",P),
   problem(P,Problem,LessThans),

   length(Problem,N),
   new_matrix(N,N,1..N,X),

   % Fill the data
   findall([I,J,Value],
           (between(1,N,I),between(1,N,J),
            matrix_element(Problem,I,J,Value),
            Value #> 0
           ),
           IJValues),
   maplist(fill_data(X),IJValues),
                        
   
   % constraints
   latin_square(X),
   maplist(less_than(X),LessThans),
   

   flatten(X,Vars),
   labeling([ff,bisect],Vars),

   print_matrix(X),
   
   nl.

fill_data(X,[I,J,V]) :-
        matrix_element(X,I,J,V).

less_than(X,[I1,J1,I2,J2]) :-
        matrix_element(X,I1,J1,X1),
        matrix_element(X,I2,J2,X2),
        X1 #< X2.



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
problem(1, Problem, LessThan) :-
        Problem =
        [[0,0,3,2,0],           % problem grid
         [0,0,0,0,0],
         [0,0,0,0,0],
         [0,0,0,0,0],
         [0,0,0,0,0]],
        
        LessThan = 
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
         [5,5,4,5]].


% Example from http://en.wikipedia.org/wiki/Futoshiki
% Solution:
% 5 4 3 2 1
% 4 3 1 5 2
% 2 1 4 3 5
% 3 5 2 1 4
% 1 2 5 4 3
%
problem(2, Problem, LessThan) :-
        Problem =
        [[0,0,0,0,0],
         [4,0,0,0,2],
         [0,0,4,0,0],
         [0,0,0,0,4],
         [0,0,0,0,0]],
        LessThan =
        [[1,2, 1,1],
         [1,4, 1,3],
         [1,5, 1,4],
         [4,4, 4,5],
         [5,1, 5,2],
         [5,2, 5,3]].


% From http://www.sudoku-puzzles.net/futoshiki06x6.html
problem(3, Problem, LessThan) :-
        Problem = 
        [[5,0,2,0,1,0],
         [6,0,1,0,0,0],
         [0,0,3,0,0,0],
         [0,0,4,0,0,0],
         [0,0,0,4,2,5],
         [0,0,0,1,3,0]],
        LessThan = 
        [[1,6, 2,6],
         [4,1, 3,1]].
