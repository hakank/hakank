/*

  Survo puzzle in SWI Prolog

  http://en.wikipedia.org/wiki/Survo_Puzzle
  """
  Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
  by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
  Survo system which is a general environment for statistical computing and 
  related areas.
  
  In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
  that each of these numbers appears only once and their row and column sums are 
  equal to integers given on the bottom and the right side of the table. 
  Often some of the integers are given readily in the table in order to 
  guarantee uniqueness of the solution and/or for making the task easier.
  """
  
  See also
  http://www.survo.fi/english/index.html
  http://www.survo.fi/puzzles/index.html
 
  References:
  Mustonen, S. (2006b). "On certain cross sum puzzles"
  http://www.survo.fi/papers/puzzles.pdf 
  Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
  http://www.survo.fi/papers/enum_survo_puzzles.pdf 
  Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
  http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
  R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        survo_puzzle(1),
        nl.

go2 :-
        between(1,6,P),
        time(survo_puzzle(P)),
        fail,
        nl.

go2.

survo_puzzle(P) :-
        writeln(problem=P),
        problem(P, RowSums, ColSums, Problem),
        matrix_dimensions(Problem, Rows, Cols),
        
        flatten(Problem, Vars),
        RC is Rows*Cols,
        Vars ins 1..RC,

        all_different(Vars),
        % all_distinct(Vars),        
        sums(Problem, RowSums),
        transpose(Problem, Transposed),
        sums(Transposed, ColSums),        
        
        labeling([ff,bisect], Vars),
        
        print_matrix(Problem),
        nl, nl.


sums([],_).
sums([R|Rs],[S|Ss]) :-
        sum(R,#=, S),
        sums(Rs,Ss).

%
% Data
%

% http://en.wikipedia.org/wiki/Survo_Puzzle, first example
%
% Solution:
%  12 6 2 10
%  8 1 5 4
%  7 9 3 11
%
problem(1, RowSums, ColSums, Problem) :-
        RowSums = [30,18,30],
        ColSums = [27,16,10,25],
        Problem = [[_, 6, _, _],
                   [8, _, _, _],
                   [_, _, 3, _]].



% http://en.wikipedia.org/wiki/Survo_Puzzle, second example
% difficulty 0
problem(2, RowSums, ColSums, Problem) :-
        RowSums = [9, 12],       % rowsums
        ColSums = [9, 7, 5],     % colsums
        Problem = [[_, _, 3],  % problem
                   [_, 6, _]].
        


% http://en.wikipedia.org/wiki/Survo_Puzzle, third example
% difficulty 150 ("open puzzle", i.e. no hints]
% It's an unique solution.
% (817 propagations with Gecode/fz, and 33 failures, 88 commits]
% r = 3;
% c = 4;
% rowsums = [24,15,39];
% colsums = [21,10,18,29];
% matrix = array2d(1..r, 1..c, 
%   [
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0
%   ]];
% Note: this version has no hints
problem(3, RowSums, ColSums, Problem) :-
        RowSums = [24,15,39],      % rowsums
        ColSums = [21,10,18,29],   % colsums
        Problem = [[_, _, _, _], % problem
                   [_, _, _, _],
                   [_, _, _, _]].




% same as above but with hints: difficulty 0
% (15 propagations with Gecode/fz, no failures, no commits]
% matrix = array2d(1..r, 1..c, 
%    [
%      7, 0, 5, 0,
%      0, 1, 0, 8,
%      0, 0, 11, 0
%    ]];
problem(4, RowSums, ColSums, Problem) :- 
       RowSums = [24,15,39],      % rowsums
       ColSums = [21,10,18,29],   % colsums
       Problem = [[7, _, 5, _], % problem
                  [_, 1, _, 8],
                  [_, _, 11, _]].



% http://www.survo.fi/puzzles/280708.txt, third puzzle
% Survo puzzle 128/2008 (1700] #364-35846
%
%    A  B  C  D  E  F
% 1  *  *  *  *  *  * 30
% 2  *  * 18  *  *  * 86
% 3  *  *  *  *  *  * 55
%   22 11 42 32 27 37
problem(5, RowSums, ColSums, Problem) :-
       RowSums = [30, 86, 55],
       ColSums = [22, 11, 42, 32, 27, 37],
       Problem = [[_, _,  _, _, _, _],
                  [_, _, 18, _, _, _],
                  [_, _,  _, _, _, _]].

%
% http://en.wikipedia.org/wiki/Survo_Puzzle, under "Swapping method"
% (open puzzle]
%
problem(6, RowSums, ColSums, Problem) :-
       RowSums = [51,36,32,17],
       ColSums = [51,42,26,17],
       Problem = [[_, _, _, _],
                  [_, _, _, _],
                  [_, _, _, _],
                  [_, _, _, _]].
