/*

  Minesweeper solver in B-Prolog.

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
        findall(_, time2(minesweeper(0)), _),
        findall(_, time2(minesweeper(1)), _),
        findall(_, time2(minesweeper(2)), _),
        findall(_, time2(minesweeper(3)), _),
        findall(_, time2(minesweeper(4)), _),
        findall(_, time2(minesweeper(5)), _),
        findall(_, time2(minesweeper(6)), _),
        findall(_, time2(minesweeper(7)), _),
        findall(_, time2(minesweeper(8)), _),
        findall(_, time2(minesweeper(9)), _),
        findall(_, time2(minesweeper(10)), _),
        findall(_, time2(minesweeper(11)), _), % 4 solutions
        findall(_, time2(minesweeper(12)), _), % 2 solutions
        once(time2(minesweeper(13))), % many, many solutions. Show just the first.
        findall(_, time2(minesweeper(14)), _).


% special for problem 13 (which has _many_ solutions)
go2 :-
        time2(minesweeper(13)), fail.


%
% Main Minesweeper solver
%
minesweeper(Problem) :-

        problem(Problem, Game),
        format("\nPROBLEM ~d\n", [Problem]),
        
        % dimensions of the problem instance
        R @= Game^length,
        C @= Game[1]^length,

        % decision variable: where is the mines?
        new_array(Mines, [R,C]),
        array_to_list(Mines, MinesVar),
        MinesVar :: 0..1,

        %
        % Game[I,J] = _ means that it is unknown from start, may be a mine.
        % Games[I,J] >= 0 means that the value is known and that it is not a mine.
        %
        foreach(I in 1..R, J in 1..C, 
                [GameIJ, MinesIJ],
                (
                  GameIJ @= Game[I,J],
                  MinesIJ @= Mines[I,J],

                  % some reasoning about this cell
                  (ground(GameIJ) -> MinesIJ #= 0 ; true),
                  (MinesIJ #= 1 -> ground(GameIJ) ; true),

                  % we check only those cells we are unsure of, i.e.
                  % when GameIJ >= 0
                  ground(GameIJ) 
                ->
                  % sum the number of neighbors of this cell
                  % all sums must sum up to Game[I,J]
                  GameIJ #= sum([ MinesIAJB :  A in -1..1, B in -1..1,
                                  [MinesIAJB],
                                  (
                                    I+A #>  0, J+B #>  0,
                                    I+A #=< R, J+B #=< C ->
                                    MinesIAJB @= Mines[I+A,J+B]
                                  )
                                ])
                ;
                  true
                )
               ),

        % search
        labeling(MinesVar),
        
        % print
        pretty_print(Mines),
        nl.


pretty_print(X) :-
        foreach(I in 1..X^length,
                (
                    foreach(J in 1..X[1]^length,
                            ( X[I,J] =:= 1 -> write('X') ; write('_') )
                           ),
                    nl
                )
               ).

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
%
problem(0,[]([](_,_,2,_,3,_),
             [](2,_,_,_,_,_),
             [](_,_,2,4,_,3),
             [](1,_,3,4,_,_),
             [](_,_,_,_,_,3),
             [](_,3,_,3,_,_))).



% Problem from Gecode/examples/minesweeper.cc  problem 1
problem(1,[]([](_,2,_,2,1,1,_,_),
             [](_,_,4,_,2,_,_,2),
             [](2,_,_,2,_,_,3,_),
             [](2,_,2,2,_,3,_,3),
             [](_,_,1,_,_,_,4,_),
             [](1,_,_,_,2,_,_,3),
             [](_,2,_,2,2,_,3,_),
             [](1,_,1,_,_,1,_,1))).



% Problem from Gecode/examples/minesweeper.cc  problem 2
problem(2,[]([](1,_,_,2,_,2,_,2,_,_),
             [](_,3,2,_,_,_,4,_,_,1),
             [](_,_,_,1,3,_,_,_,4,_),
             [](3,_,1,_,_,_,3,_,_,_),
             [](_,2,1,_,1,_,_,3,_,2),
             [](_,3,_,2,_,_,2,_,1,_),
             [](2,_,_,3,2,_,_,2,_,_),
             [](_,3,_,_,_,3,2,_,_,3),
             [](_,_,3,_,3,3,_,_,_,_),
             [](_,2,_,2,_,_,_,2,2,_))).


% Problem from Gecode/examples/minesweeper.cc  problem 3
problem(3,[]([](2,_,_,_,3,_,1,_),
             [](_,5,_,4,_,_,_,1),
             [](_,_,5,_,_,4,_,_),
             [](2,_,_,_,4,_,5,_),
             [](_,2,_,4,_,_,_,2),
             [](_,_,5,_,_,4,_,_),
             [](2,_,_,_,5,_,4,_),
             [](_,3,_,3,_,_,_,2))).


% Problem from Gecode/examples/minesweeper.cc  problem 4
problem(4,[]([](0,_,0,_,1,_,_,1,1,_),
             [](1,_,2,_,2,_,2,2,_,_),
             [](_,_,_,_,_,_,2,_,_,2),
             [](_,2,3,_,1,1,_,_,_,_),
             [](0,_,_,_,_,_,_,2,_,1),
             [](_,_,_,2,2,_,1,_,_,_),
             [](_,_,_,_,_,3,_,3,2,_),
             [](_,5,_,2,_,_,_,3,_,1),
             [](_,3,_,1,_,_,3,_,_,_),
             [](_,2,_,_,_,1,2,_,_,0))).


% Problem from Gecode/examples/minesweeper.cc  problem 5
problem(5,[]([](_,2,1,_,2,_,2,_,_,_),
             [](_,4,_,_,3,_,_,_,5,3),
             [](_,_,_,4,_,4,4,_,_,3),
             [](4,_,4,_,_,5,_,6,_,_),
             [](_,_,4,5,_,_,_,_,5,4),
             [](3,4,_,_,_,_,5,5,_,_),
             [](_,_,4,_,4,_,_,5,_,5),
             [](2,_,_,3,3,_,6,_,_,_),
             [](3,6,_,_,_,3,_,_,4,_),
             [](_,_,_,4,_,2,_,2,1,_))).



% Problem from Gecode/examples/minesweeper.cc  problem 6
problem(6,[]([](_,3,2,_,_,1,_,_),
             [](_,_,_,_,1,_,_,3),
             [](3,_,_,2,_,_,_,4),
             [](_,5,_,_,_,5,_,_),
             [](_,_,6,_,_,_,5,_),
             [](3,_,_,_,5,_,_,4),
             [](2,_,_,5,_,_,_,_),
             [](_,_,2,_,_,3,4,_))).


% Problem from Gecode/examples/minesweeper.cc  problem 7
problem(7,[]([](_,1,_,_,_,_,_,3,_),
             [](_,_,_,3,4,3,_,_,_),
             [](2,4,4,_,_,_,4,4,3),
             [](_,_,_,4,_,4,_,_,_),
             [](_,4,_,4,_,3,_,6,_),
             [](_,_,_,4,_,3,_,_,_),
             [](1,2,3,_,_,_,1,3,3),
             [](_,_,_,3,2,2,_,_,_),
             [](_,2,_,_,_,_,_,3,_))).



% Problem from Gecode/examples/minesweeper.cc  problem 8
problem(8,[]([](_,_,_,_,_,_,_),
             [](_,2,3,4,3,5,_),
             [](_,1,_,_,_,3,_),
             [](_,_,_,5,_,_,_),
             [](_,1,_,_,_,3,_),
             [](_,1,2,2,3,4,_),
             [](_,_,_,_,_,_,_))).


% Problem from Gecode/examples/minesweeper.cc  problem 9
problem(9,[]([](2,_,_,_,2,_,_,_,2),
             [](_,4,_,4,_,3,_,4,_),
             [](_,_,4,_,_,_,1,_,_),
             [](_,4,_,3,_,3,_,4,_),
             [](2,_,_,_,_,_,_,_,2),
             [](_,5,_,4,_,5,_,4,_),
             [](_,_,3,_,_,_,3,_,_),
             [](_,4,_,3,_,5,_,6,_),
             [](2,_,_,_,1,_,_,_,2))).



% From "Some Minesweeper Configurations",page 2
problem(10,[]([](_,_,_,_,_,_),
              [](_,2,2,2,2,_),
              [](_,2,0,0,2,_),
              [](_,2,0,0,2,_),
              [](_,2,2,2,2,_),
              [](_,_,_,_,_,_))).



% From "Some Minesweeper Configurations",page 3
% 4 solutions
problem(11,[]([](2,3,_,2,2,_,2,1),
              [](_,_,4,_,_,4,_,2),
              [](_,_,_,_,_,_,4,_),
              [](_,5,_,6,_,_,_,2),
              [](2,_,_,_,5,5,_,2),
              [](1,3,4,_,_,_,4,_),
              [](0,1,_,4,_,_,_,3),
              [](0,1,2,_,2,3,_,2))).


% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
% 
% A Wire,page 33
% 2 solutions
%
problem(12,[]([](_,0,0,0,0,0,0,0,0,0,0,0,0,_),
              [](_,1,1,1,1,1,1,1,1,1,1,1,1,_),
              [](_,_,1,_,_,1,_,_,1,_,_,1,_,_),
              [](_,1,1,1,1,1,1,1,1,1,1,1,1,_),
              [](_,0,0,0,0,0,0,0,0,0,0,0,0,_))).


% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
% A splitter,page 35
% Many solutions...
%
problem(13,[]([](_,_,_,0,_,_,_,0,_,_,_),
              [](_,_,_,0,1,_,1,0,_,_,_),
              [](_,_,_,0,1,_,1,0,_,_,_),
              [](0,0,0,0,1,1,1,0,0,0,0),
              [](_,1,1,1,1,_,1,1,1,1,_),
              [](_,_,_,1,_,2,_,1,_,_,_),
              [](_,1,1,1,1,_,1,1,1,1,_),
              [](0,0,0,0,1,1,1,0,0,0,0),
              [](_,_,_,0,1,_,1,0,_,_,_),
              [](_,_,_,0,1,_,1,0,_,_,_),
              [](_,_,_,0,_,_,_,0,_,_,_))).
        


% Oleg German,Evgeny Lakshtanov: "Minesweeper" without a computer
% http://arxiv.org/abs/0806.3480, page 4
problem(14,[]([](_,1,_,1,_,1),
              [](2,_,2,_,1,_),
              [](_,3,_,2,_,1),
              [](1,_,3,_,2,_),
              [](_,1,_,2,_,1))).

