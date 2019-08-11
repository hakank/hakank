/*

  Minesweeper solver in SWI Prolog

  From gecode/examples/minesweeper.cc:
  """
  A specification is a square matrix of characters. Alphanumeric
  characters represent the number of mines adjacent to that field. 
  Dots represent fields with an unknown number of mines adjacent to 
  it (or an actual mine).
  """
  
  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """
  
  Also see:
  * http://www.janko.at/Raetsel/Minesweeper/index.htm

  * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
 
  * Ian Stewart on Minesweeper: 
    http://www.claymath.org/Popular_Lectures/Minesweeper/

  * Richard Kaye's Minesweeper Pages
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm

  * Some Minesweeper Configurations
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        minesweeper(1),
        nl.

%%
%% Problems 1..10, and 14: has unique solutions
%%          11: 4 solutions
%%          12: 2 solutions
%%          13: too many, many solutions
%%          15: 20 solutions
%%
go2 :-
        between(1,15,P),
        P \= 13,
        findall(_, time(minesweeper(P)),L),
        length(L,Len),
        writeln([problem=P,number_of_solutions=Len]),
        nl,
        fail,
        nl,
        nl.



% special for problem 13 (which has _many_ solutions)
go3 :-
  time(minesweeper(13)), fail.


% special for problem 15
% 20 solutions
go4 :-
        writeln(problem=15),
        time(findall(_, minesweeper(15),L)),
        length(L,Len),
        writeln([number_of_solutions=Len]),
        nl.



%
% Main Minesweeper solver
%
minesweeper(Problem) :-

   problem(Problem, Game),
   writeln(problem=Problem),
   
   % dimensions of the problem instance
   length(Game, NumRows),
   transpose(Game, GameT),
   length(GameT, NumCols),

   %% decision variable: where is the mines?
   %% Mines[I,J] = 1: this cell has a mine. 
   new_matrix(NumRows, NumCols, 0..1, Mines),

   %% Main loop
   findall([I,J], (between(1,NumRows,I), between(1,NumCols,J)),GridIndices),
   grid_loop(GridIndices, Game, Mines, NumRows, NumCols), 

   flatten(Mines, Vars),
   
   labeling([ff,bisect], Vars),

   print_matrix(Mines),
   nl.

%%
%% Main loop,
%%
%% Game[I,J] = _ means that it is unknown from start, may be a mine.
%% Games[I,J] >= 0 means that the value is known, (i.e. the number of
%%                 mines in the neighbourhood) and also that it is not a mine.
%%
%%
grid_loop([], _Game, _Mines, _NumRows, _NumCols).
grid_loop([[I,J]|Gs], Game, Mines, NumRows, NumCols) :-
        matrix_element(Game,I,J, NumMines), % Game[I,J], i.e. the number of Mines in this cell
        neighbours(Mines, NumRows, NumCols, I,J, NumMines),
        grid_loop(Gs, Game, Mines, NumRows, NumCols).

%%
%% Ensure that among the neighbours of M[I,J] there is exactly NumMines mines
%%
neighbours(Mines, NumRows, NumCols, I,J, NumMines) :-
        % unknown number of mines. Can be a mine.
        (var(NumMines) ->
            true
        ;
            % If Game[I,J] is 0 or > 0 then this cannot be a mine.
            matrix_element(Mines,I,J,0),
            % And now we ensure that there are NumMines mines in the neighbourhood
            neighbour_list(NumRows, NumCols, I,J, NeighbourList),
            sum_neighbours(NeighbourList, Mines, NumMines)
            
        ).

%%
%% The indices of the neighbours for [I,J] in a Rows x Cols matrix
%%
neighbour_list(NumRows, NumCols, I,J, NeighbourList) :-
        findall([IA,JB],
                (
                  between(-1,1,A),
                  between(-1,1,B),
                  IA#=I+A, JB#=J+B,
                  IA in 1..NumRows,
                  JB in 1..NumCols
                ),
                NeighbourList).

%%
%% The number of mines in the neighbours is NumMines
%%
sum_neighbours(NeighbourList, Mines, NumMines) :-
        sum_neighbours_(NeighbourList,Mines, 0, NumMines).
sum_neighbours_([], _Mines, Sum, NumMines) :-
        Sum #= NumMines.
sum_neighbours_([[I,J]|Ns],Mines, Sum, NumMines) :-
          matrix_element(Mines,I,J, Val),
          Sum1 #= Val + Sum,
          sum_neighbours_(Ns,Mines, Sum1, NumMines).


%
% data
%
%  _ is coded as unknown
%  0..8: known number of neighbours
%

% The first 10 examples (0..9) are from Gecode/examples/minesweeper.cc
% http://www.gecode.org/gecode-doc-latest/minesweeper_8cc-source.html
% """
% The instances are taken from
%   http://www.janko.at/Raetsel/Minesweeper/index.htm
% """


% Problem from Gecode/examples/minesweeper.cc  problem 0
% 
% Solution:
%  1 0 0 0 0 1
%  0 1 0 1 1 0
%  0 0 0 0 1 0
%  0 0 0 0 1 0
%  0 1 1 1 0 0
%  1 0 0 0 1 1
problem(0, P) :- 
        P = [[_,_,2,_,3,_],
             [2,_,_,_,_,_],
             [_,_,2,4,_,3],
             [1,_,3,4,_,_],
             [_,_,_,_,_,3],
             [_,3,_,3,_,_]].


% Problem from Gecode/examples/minesweeper.cc  problem 1
problem(1, P) :- 
        P = [[_,2,_,2,1,1,_,_],
             [_,_,4,_,2,_,_,2],
             [2,_,_,2,_,_,3,_],
             [2,_,2,2,_,3,_,3],
             [_,_,1,_,_,_,4,_],
             [1,_,_,_,2,_,_,3],
             [_,2,_,2,2,_,3,_],
             [1,_,1,_,_,1,_,1]].



% Problem from Gecode/examples/minesweeper.cc  problem 2
problem(2,P) :- 
        P = [[1,_,_,2,_,2,_,2,_,_],
             [_,3,2,_,_,_,4,_,_,1],
             [_,_,_,1,3,_,_,_,4,_],
             [3,_,1,_,_,_,3,_,_,_],
             [_,2,1,_,1,_,_,3,_,2],
             [_,3,_,2,_,_,2,_,1,_],
             [2,_,_,3,2,_,_,2,_,_],
             [_,3,_,_,_,3,2,_,_,3],
             [_,_,3,_,3,3,_,_,_,_],
             [_,2,_,2,_,_,_,2,2,_]].


% Problem from Gecode/examples/minesweeper.cc  problem 3
problem(3, P) :- 
        P = [[2,_,_,_,3,_,1,_],
             [_,5,_,4,_,_,_,1],
             [_,_,5,_,_,4,_,_],
             [2,_,_,_,4,_,5,_],
             [_,2,_,4,_,_,_,2],
             [_,_,5,_,_,4,_,_],
             [2,_,_,_,5,_,4,_],
             [_,3,_,3,_,_,_,2]].


% Problem from Gecode/examples/minesweeper.cc  problem 4
problem(4,P) :- 
        P = [[0,_,0,_,1,_,_,1,1,_],
             [1,_,2,_,2,_,2,2,_,_],
             [_,_,_,_,_,_,2,_,_,2],
             [_,2,3,_,1,1,_,_,_,_],
             [0,_,_,_,_,_,_,2,_,1],
             [_,_,_,2,2,_,1,_,_,_],
             [_,_,_,_,_,3,_,3,2,_],
             [_,5,_,2,_,_,_,3,_,1],
             [_,3,_,1,_,_,3,_,_,_],
             [_,2,_,_,_,1,2,_,_,0]].


% Problem from Gecode/examples/minesweeper.cc  problem 5
problem(5,P) :- 
        P = [[_,2,1,_,2,_,2,_,_,_],
             [_,4,_,_,3,_,_,_,5,3],
             [_,_,_,4,_,4,4,_,_,3],
             [4,_,4,_,_,5,_,6,_,_],
             [_,_,4,5,_,_,_,_,5,4],
             [3,4,_,_,_,_,5,5,_,_],
             [_,_,4,_,4,_,_,5,_,5],
             [2,_,_,3,3,_,6,_,_,_],
             [3,6,_,_,_,3,_,_,4,_],
             [_,_,_,4,_,2,_,2,1,_]].



% Problem from Gecode/examples/minesweeper.cc  problem 6
problem(6, P) :- 
        P = [[_,3,2,_,_,1,_,_],
             [_,_,_,_,1,_,_,3],
             [3,_,_,2,_,_,_,4],
             [_,5,_,_,_,5,_,_],
             [_,_,6,_,_,_,5,_],
             [3,_,_,_,5,_,_,4],
             [2,_,_,5,_,_,_,_],
             [_,_,2,_,_,3,4,_]].


% Problem from Gecode/examples/minesweeper.cc  problem 7
problem(7, P) :- 
        P = [[_,1,_,_,_,_,_,3,_],
             [_,_,_,3,4,3,_,_,_],
             [2,4,4,_,_,_,4,4,3],
             [_,_,_,4,_,4,_,_,_],
             [_,4,_,4,_,3,_,6,_],
             [_,_,_,4,_,3,_,_,_],
             [1,2,3,_,_,_,1,3,3],
             [_,_,_,3,2,2,_,_,_],
             [_,2,_,_,_,_,_,3,_]].



% Problem from Gecode/examples/minesweeper.cc  problem 8
problem(8, P) :- 
        P = [[_,_,_,_,_,_,_],
             [_,2,3,4,3,5,_],
             [_,1,_,_,_,3,_],
             [_,_,_,5,_,_,_],
             [_,1,_,_,_,3,_],
             [_,1,2,2,3,4,_],
             [_,_,_,_,_,_,_]].


% Problem from Gecode/examples/minesweeper.cc  problem 9
problem(9, P) :- 
        P = [[2,_,_,_,2,_,_,_,2],
             [_,4,_,4,_,3,_,4,_],
             [_,_,4,_,_,_,1,_,_],
             [_,4,_,3,_,3,_,4,_],
             [2,_,_,_,_,_,_,_,2],
             [_,5,_,4,_,5,_,4,_],
             [_,_,3,_,_,_,3,_,_],
             [_,4,_,3,_,5,_,6,_],
             [2,_,_,_,1,_,_,_,2]].



% From "Some Minesweeper Configurations",page 2
problem(10, P) :- 
         P = [[_,_,_,_,_,_],
              [_,2,2,2,2,_],
              [_,2,0,0,2,_],
              [_,2,0,0,2,_],
              [_,2,2,2,2,_],
              [_,_,_,_,_,_]].



% From "Some Minesweeper Configurations",page 3
% 4 solutions
problem(11, P) :- 
         P = [[2,3,_,2,2,_,2,1],
              [_,_,4,_,_,4,_,2],
              [_,_,_,_,_,_,4,_],
              [_,5,_,6,_,_,_,2],
              [2,_,_,_,5,5,_,2],
              [1,3,4,_,_,_,4,_],
              [0,1,_,4,_,_,_,3],
              [0,1,2,_,2,3,_,2]].


% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
% 
% A Wire,page 33
% 2 solutions
%
problem(12, P) :- 
         P = [[_,0,0,0,0,0,0,0,0,0,0,0,0,_],
              [_,1,1,1,1,1,1,1,1,1,1,1,1,_],
              [_,_,1,_,_,1,_,_,1,_,_,1,_,_],
              [_,1,1,1,1,1,1,1,1,1,1,1,1,_],
              [_,0,0,0,0,0,0,0,0,0,0,0,0,_]].


% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
% A splitter,page 35
% Many solutions...
%
problem(13, P) :- 
          P= [[_,_,_,0,_,_,_,0,_,_,_],
              [_,_,_,0,1,_,1,0,_,_,_],
              [_,_,_,0,1,_,1,0,_,_,_],
              [0,0,0,0,1,1,1,0,0,0,0],
              [_,1,1,1,1,_,1,1,1,1,_],
              [_,_,_,1,_,2,_,1,_,_,_],
              [_,1,1,1,1,_,1,1,1,1,_],
              [0,0,0,0,1,1,1,0,0,0,0],
              [_,_,_,0,1,_,1,0,_,_,_],
              [_,_,_,0,1,_,1,0,_,_,_],
              [_,_,_,0,_,_,_,0,_,_,_]].
        


% Oleg German,Evgeny Lakshtanov: "Minesweeper" without a computer
% http://arxiv.org/abs/0806.3480, page 4
problem(14, P) :- 
         P = [[_,1,_,1,_,1],
              [2,_,2,_,1,_],
              [_,3,_,2,_,1],
              [1,_,3,_,2,_],
              [_,1,_,2,_,1]].


%
% From http://stephenlombardi.com/minesweeper/minesweeper.lisp
%
problem(15,P) :-
  P = 
[[0,0,0,0,1,_,_,_,_],
 [0,0,0,0,1,_,_,3,_],
 [0,0,1,1,2,_,_,_,_],
 [0,0,1,_,_,_,_,1,_],
 [0,0,1,2,_,3,_,_,_],
 [0,0,0,1,_,_,_,_,_],
 [0,0,1,2,2,1,1,1,1],
 [0,0,1,_,1,0,0,0,0],
 [0,0,1,_,1,0,0,0,0]].
