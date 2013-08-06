/*

  Quasigroup completion problem in B-Prolog.

  See 
  Carla P. Gomes and David Shmoys:
 "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"

  
  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  """
  Using only the numbers 1, 2, 3, and 4, arrange four sets of these 
  numbers into a four-by-four array so that no column or row contains 
  the same two numbers. The result is known as a Latin square.
  ...
  The so-called quasigroup completion problem concerns a table that is 
  correctly but only partially filled in. The question is whether the 
  remaining blanks in the table can be filled in to obtain a complete 
  Latin square (or a proper quasigroup multiplication table).
  """


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

%
% Test the first two problems.
%
go :-
        time2(solve(1,Problem1)),
        pretty_print(Problem1),

        time2(solve(2,Problem2)),
        pretty_print(Problem2).

%
% Test all problems.
%
go2 :-
        foreach(I in 1..9, 
                [B],
                (
                    time2(solve(I,B)) ->
                        pretty_print(B)
                ;
                        true
                )
               ).

pretty_print(X) :-
        N @= X^length,
        foreach(I in 1..N,
                (
                foreach(J in 1..N,
                        [T],
                        (
                            T @= X[I,J],
                            format(' ~q ', [T])
                        )
                       ),
                nl
                )
               ),
        nl.        


%
% Ensure a Latin square, 
% i.e. all rows and all columns are different
%
alldifferent_matrix(Board) :-
        Rows @= Board^rows,
        foreach(Row in Rows, alldifferent(Row)),
        Columns @= Board^columns,
        foreach(Column in Columns, alldifferent(Column)).


solve(ProblemNum,Problem) :-
        problem(ProblemNum, Problem),
        writeln(problem:ProblemNum),
        N @= Problem^length,
        ProblemVar @= [ P : I in 1..N, J in 1..N, [P], P @= Problem[I,J]],
        ProblemVar :: 1..N,
        alldifferent_matrix(Problem),

        labeling([ff,down],ProblemVar).



%
% Example from Ruben Martins and Inès Lynce
% Breaking Local Symmetries in Quasigroup Completion Problems, page 3
% The solution is unique:
% 1 3 2 5 4
% 2 5 4 1 3
% 4 1 3 2 5
% 5 4 1 3 2
% 3 2 5 4 1
%
% Note: this is an array of arrays
%
problem(1, []([](1, _, _, _, 4),  
              [](_, 5, _, _, _),
              [](4, _, _, 2, _),
              [](_, 4, _, _, _),
              [](_, _, 5, _, 1))).


%
% Example from Gomes & Shmoys, page 3.
% Solution:
% 4 1 2 3
% 2 3 4 1
% 1 4 3 2
% 3 2 1 4
%
problem(2, []([](_, 1, 2, 3),
              [](2, _, 4, 1), 
              [](1, 4, _, 2),
              [](3, _, 1, _))).

% Example from Gomes & Shmoys, page 7
% Two solutions.
%
problem(3, []([](_, 1, _, _),
              [](_, _, 2, _),
              [](_, 3, _, _),
              [](_, _, _, 4))).


%
% Example from Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/sec2.7.108.html
%
% 12 solutions.
%
problem(4, []([](1, _, _, _),
              [](_, _, _, 3),
              [](3, _, _, _),
              [](_, _, _, 1))).


%
% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #1. 
% There are _many_ solutions to this problem.
%
problem(5, []([](_,_,_,1,_,_,_,_,_,_),
              [](_,_,1,_,_,_,_,_,_,_),
              [](_,1,_,_,_,2,_,_,_,_),
              [](1,_,_,_,2,_,_,_,_,_),
              [](_,_,_,2,1,_,_,_,_,_),
              [](_,_,2,_,_,1,_,_,_,_),
              [](_,_,_,_,_,_,1,_,_,_),
              [](_,_,_,_,_,_,_,1,_,2),
              [](_,_,_,_,_,_,_,_,2,_),
              [](_,_,_,_,_,_,_,2,_,_))).


%
% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #2. 
% There are _many_ solutions to this problem.
%
problem(6, []([](_,_,1,2,3,4,_,_,_,_),
              [](_,1,2,3,_,_,4,_,_,_),
              [](1,2,3,_,_,_,_,4,_,_),
              [](2,3,_,_,_,_,_,_,4,_),
              [](3,_,_,_,_,_,_,_,_,4),
              [](5,6,_,_,_,_,_,_,_,_),
              [](_,5,6,_,_,_,_,_,_,_),
              [](_,_,5,6,_,_,_,_,_,_),
              [](_,_,_,5,6,_,_,_,_,_),
              [](_,_,_,_,5,6,_,_,_,_))).


%
% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #3. 
% Coding:
%    dark red   = 1
%    light blue = 2 
%    dark blue  = 3 
%    light red  = 4
%    brown      = 5
%    green      = 6
%    pink       = 7
%    grey       = 8
%    black      = 9
%    yellow     = 10    
% There are 40944 solutions for this pattern.
%
problem(7, []([](_, _, 1, 5, 2, 6, 7, 8, _, _),
              [](_, 1, 5, 2, _, _, 6, 7, 8, _),
              [](1, 5, 2, _, _, _, _, 6, 7, 8),
              [](5, 2, _, _, _, _, _, _, 6, 7),
              [](2, _, _, _, _, _, _, _, _, 6),
              [](4,10, _, _, _, _, _, _, 3, 9),
              [](_, 4,10, _, _, _, _, 3, 9, _),
              [](_, _, 4,10, _, _, 3, 9, _, _),
              [](_, _, _, 4,10, 3, 9, _, _, _), 
              []( _, _, _, _, 4,9, _, _, _, _))).


%
% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #4. 
%  dark red   = 1
%  light blue = 2
%  dark blue  = 3
%  light red  = 4
% Note: There are no solutions to this problem.
%
problem(8, []([](1,_,_,_,_,_,_,_,_,_),
              [](2,1,_,_,_,_,_,_,_,4),
              [](3,2,1,_,_,_,_,_,4,_),
              [](_,3,2,1,_,_,_,4,_,_),
              [](_,_,3,2,1,_,4,_,_,_),
              [](_,_,_,3,2,1,_,_,_,_),
              [](_,_,_,_,3,2,1,_,_,_),
              [](_,_,_,4,_,3,2,1,_,_),
              [](_,_,4,_,_,_,3,2,1,_),
              [](_,4,_,_,_,_,_,3,2,1))).


%
% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #5
% Note: There are no solutions to this problem.
%
problem(9, []([](_,_,_,_,_,_,_,_,_,1),
              [](_,_,_,_,_,_,_,_,1,_),
              [](_,_,_,_,_,_,_,1,_,_),
              [](_,_,_,_,_,_,2,_,_,_),
              [](_,_,_,_,_,1,_,_,_,_),
              [](_,_,_,_,1,_,_,_,_,_),
              [](_,_,_,1,_,_,_,_,_,_),
              [](_,_,1,_,_,_,_,_,_,_),
              [](_,1,_,_,_,_,_,_,_,_),
              [](1,_,_,_,_,_,_,_,_,_))).


