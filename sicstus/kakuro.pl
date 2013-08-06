/*

  Kakuru puzzle in SICStus Prolog.

 http://en.wikipedia.org/wiki/Kakuro
  """
  The object of the puzzle is to insert a digit from 1 to 9 inclusive 
  into each white cell such that the sum of the numbers in each entry 
  matches the clue associated with it and that no digit is duplicated in 
  any entry. It is that lack of duplication that makes creating Kakuro 
  puzzles with unique solutions possible, and which means solving a Kakuro 
  puzzle involves investigating combinations more, compared to Sudoku in 
  which the focus is on permutations. There is an unwritten rule for 
  making Kakuro puzzles that each clue must have at least two numbers 
  that add up to it. This is because including one number is mathematically 
  trivial when solving Kakuro puzzles; one can simply disregard the 
  number entirely and subtract it from the clue it indicates.
  """

  This model solves the problem at the Wikipedia page. 
  For a larger picture, see
  http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg

  The solution:
    9 7 0 0 8 7 9
    8 9 0 8 9 5 7
    6 8 5 9 7 0 0
    0 6 1 0 2 6 0
    0 0 4 6 1 3 2
    8 9 3 1 0 1 4
    3 1 2 0 0 2 1



  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/kakuro.co
  * MiniZinc: http://www.hakank.org/minizinc/kakuro.mzn

  * SICStus : http://www.hakank.org/sicstus/kenken2.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :- 
  problem(P, N, Hints, Blanks),

  format('Kakuro problem ~d\n',[P]),

  matrix(X,[N,N]),
  append(X,XList),
  domain(XList,0,9),

  % Fill the blanks
  ( foreach([RR,CC],Blanks),
    param(X) do
        matrix_element(X,RR,CC,0)
  ),

  % The hints
  ( foreach([Sum|List],Hints),
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


  labeling([ffc,enum,up],XList),

  ( foreach(Row,X) do write(Row), nl ), nl,
  fd_statistics.


% problem(Id, Size, Hints, Blanks).
problem(1,7,
  [ % [Sum, [List of indices in X]]
      [16, [1,1],[1,2]],
      [24, [1,5],[1,6],[1,7]],
      [17, [2,1],[2,2]],
      [29, [2,4],[2,5],[2,6],[2,7]],
      [35, [3,1],[3,2],[3,3],[3,4],[3,5]],
      [ 7, [4,2],[4,3]],
      [ 8, [4,5],[4,6]],
      [16, [5,3],[5,4],[5,5],[5,6],[5,7]],
      [21, [6,1],[6,2],[6,3],[6,4]],
      [ 5, [6,6],[6,7]],
      [ 6, [7,1],[7,2],[7,3]],
      [ 3, [7,6],[7,7]],
      
      [23, [1,1],[2,1],[3,1]],
      [30, [1,2],[2,2],[3,2],[4,2]],
      [27, [1,5],[2,5],[3,5],[4,5],[5,5]],
      [12, [1,6],[2,6]],
      [16, [1,7],[2,7]],
      [17, [2,4],[3,4]],   
      [15, [3,3],[4,3],[5,3],[6,3],[7,3]],
      [12, [4,6],[5,6],[6,6],[7,6]],
      [ 7, [5,4],[6,4]],   
      [ 7, [5,7],[6,7],[7,7]],
      [11, [6,1],[7,1]],
      [10, [6,2],[7,2]]
  ],

   % inices of blanks
   [
       [1,3], [1,4],
       [2,3],
       [3,6], [3,7],
       [4,1], [4,4],[4,7],
       [5,1], [5,2],
       [6,5],
       [7,4], [7,5]
   ]).


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

