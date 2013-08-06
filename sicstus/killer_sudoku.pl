/*

  Killer Sudoku in SICStus Prolog.

  http://en.wikipedia.org/wiki/Killer_Sudoku
  """
  Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or 
  samunamupure) is a puzzle that combines elements of sudoku and kakuro. 
  Despite the name, the simpler killer sudokus can be easier to solve 
  than regular sudokus, depending on the solver's skill at mental arithmetic; 
  the hardest ones, however, can take hours to crack.

  ...

  The objective is to fill the grid with numbers from 1 to 9 in a way that 
  the following conditions are met:

    * Each row, column, and nonet contains each number exactly once.
    * The sum of all numbers in a cage must match the small number printed 
      in its corner.
    * No number appears more than once in a cage. (This is the standard rule 
      for killer sudokus, and implies that no cage can include more 
      than 9 cells.)

  In 'Killer X', an additional rule is that each of the long diagonals 
  contains each number once.
  """

  Here we solve the problem from the Wikipedia page, also shown here
  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

  The output is:
    2 1 5 6 4 7 3 9 8
    3 6 8 9 5 2 1 7 4
    7 9 4 3 8 1 6 5 2
    5 8 6 2 7 4 9 3 1
    1 4 2 5 9 3 8 6 7
    9 7 3 8 1 6 4 2 5
    8 2 1 7 3 9 5 4 6
    6 5 9 4 2 8 7 1 3
    4 3 7 1 6 5 2 8 9


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/killer_sudoku.co
  * MiniZinc: http://www.hakank.org/minizinc/killer_sudoku.mzn

  These models uses the same principle that is used here:
  * SICStus : http://www.hakank.org/sicstus/kenken2.pl 
  * SICStus : http://www.hakank.org/sicstus/kakuro.pl 
  * SICStus : http://www.hakank.org/sicstus/sudoku.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :- 
        problem(1,Hints),
        killer_sudoku(Hints,X),
        ( foreach(Row,X)
        do
          write(Row),nl
        ),
        nl,
        fd_statistics.

killer_sudoku(Problem,X) :-
        
        matrix(X,[9,9]),

        append(X, Vars), % flattening for domain and labeling
        domain(Vars, 1,9),

        % The hints
        % (This was just copied from
        %  http://www.hakank.org/sicstus/kakuro.pl )
        ( foreach([Sum|List],Problem),
          param(X) do 
              ( foreach([R,C],List),
                fromto(XLine,Out,In,[]),
                param(X) do
                    matrix_element(X,R,C,XRC),
                    XRC #> 0,
                    Out = [XRC|In]
              ),
              sum(XLine,#=,Sum),
              all_different(XLine)
        ),
        

        % These standard Sudoku constraints are 
        % from http://www.hakank.org/sicstus/sudoku.pl

        % rows
        ( foreach(Row, X)
        do
          all_different(Row)
        ),
        
        % columns
        transpose(X,Columns),
        ( foreach(Column, Columns)
        do
          all_different(Column)
        ),

        % cells
        ( for(I, 0, 2), param(Vars) do
              ( for(J, 0, 2), 
                param(I,Vars) do
                    ( for(R, I*3,I*3+2), 
                      fromto(RR, OutR, InR, []),
                      param(I,J,Vars) do
                          ( for(C, J*3,J*3+2), 
                            fromto(CC, OutC, InC, []),
                            param(I,J,R,Vars) do
                                X is 1+R*9+C,
                                element(X, Vars, El),
                                OutC = [El|InC]
                          ),
                          OutR = [CC|InR]
                    ),
                    append(RR,V),
                    all_different(V)
              )
        ),

        labeling([ffc,bisect,down], Vars).




matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% Suggested by Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



problem(1, 
        [% The hints:
         %  [Sum, [list of indices in X]]
            [ 3, [1,1], [1,2]],
            [15, [1,3], [1,4], [1,5]],
            [22, [1,6], [2,5], [2,6], [3,5]],
            [ 4, [1,7], [2,7]],
            [16, [1,8], [2,8]],
            [15, [1,9], [2,9], [3,9], [4,9]],
            [25, [2,1], [2,2], [3,1], [3,2]],
            [17, [2,3], [2,4]],
            [ 9, [3,3], [3,4], [4,4]],
            [ 8, [3,6], [4,6],[5,6]],
            [20, [3,7], [3,8],[4,7]],
            [ 6, [4,1], [5,1]],
            [14, [4,2], [4,3]],
            [17, [4,5], [5,5],[6,5]],
            [17, [4,8], [5,7],[5,8]],
            [13, [5,2], [5,3],[6,2]],
            [20, [5,4], [6,4],[7,4]],
            [12, [5,9], [6,9]],
            [27, [6,1], [7,1],[8,1],[9,1]],
            [ 6, [6,3], [7,2],[7,3]],
            [20, [6,6], [7,6], [7,7]],
            [ 6, [6,7], [6,8]],
            [10, [7,5], [8,4],[8,5],[9,4]],
            [14, [7,8], [7,9],[8,8],[8,9]],
            [ 8, [8,2], [9,2]],
            [16, [8,3], [9,3]],
            [15, [8,6], [8,7]],
            [13, [9,5], [9,6],[9,7]],
            [17, [9,8], [9,9]]
        ]).

