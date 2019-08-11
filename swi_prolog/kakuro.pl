/*

  Kakuru puzzle in SWI Prolog

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

  problem(P, N, Hints, Blanks),
  format("Kakuro problem ~d\n",P),

  new_matrix(N,N,0..9, X),

  % Fill the blanks
  maplist(fill_blanks(X), Blanks),

  %% The hints
  maplist(lines(X),Hints),

  flatten(X,Vars),
  label(Vars),

  maplist(writeln,X),
  nl.


%%
%% Handle the blanks and hints
%%
fill_blanks(X,[I,J]) :-
        matrix_element(X,I,J, 0).

lines(X,Hints) :-
        [Sum|IJs] = Hints,
        maplist(larger_than_zero(X),IJs),
        extract_from_indices2d(IJs,X,XLine),
        sum(XLine, #=, Sum),
        all_different(XLine).

%%
%% X[I,J] #> 0
%%
larger_than_zero(X,[I,J]) :-
        matrix_element(X,I,J,XIJ),
        XIJ #> 0.


print_board(Board) :-
        maplist(writeln,Board),
        nl.


%%
%% This is the problem cited above.
%%
%% problem(Id, Size, Hints, Blanks).
%%
problem(Id,Size, Hints, Blanks) :-
        Id = 1,
        Size = 7,
        %% [Sum, [List of indices in X]]
        Hints =  [ 
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
        
        %% indices of blanks
        Blanks = 
        [
         [1,3], [1,4],
         [2,3],
         [3,6], [3,7],
         [4,1], [4,4],[4,7],
         [5,1], [5,2],
         [6,5],
         [7,4], [7,5]
        ].

