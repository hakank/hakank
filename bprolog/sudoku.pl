/*

  Sudoku solver in B-Prolog.

  See http://en.wikipedia.org/wiki/Sudoku

  Some of the problem instances (and some idea) are from 
  ECLiPSe's Sudoku model: 
       http://eclipseclp.org/examples/sudoku.ecl.txt

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
% Problem 12 gives 0 backtracks
%
go :-
        time2(solve(13)).

%
% Test all problems 
%
go2 :-
        foreach(P in 1..13,
                (writeln(problem:P),
                 time2(solve(P)),
                 nl
                )
               ).

%
% Test all problems 
%
go3 :-
        foreach(P in 1..13,
                [L,Len],
                (writeln(problem:P),
                 time2(findall(P,solve2(P),L)),
                 length(L,Len),
                 writeln(len:Len),
                 (Len > 1 ->
                      writeln('This has more than one solution!') 
                 ; 
                      true
                 ),
                 nl
                )
               ).


%
% Just checking the model: It should be a unique solution, and it is.
%
go4 :-
        problem(12,Board),
        time2(findall(Board, sudoku(3,Board), All)),
        foreach(B in All, print_board(B)).



solve(ProblemName) :-
	problem(ProblemName, Board),
	print_board(Board),
	sudoku(3, Board),
	print_board(Board).


% Don't print the solution (for go4/0)
solve2(ProblemName) :-
	problem(ProblemName, Board),
	sudoku(3, Board).


%
% Ensure a Latin square, 
% i.e. all rows and all columns are different
%
alldifferent_matrix(Board) :-
        Rows @= Board^rows,
        foreach(Row in Rows, alldifferent(Row)),
        Columns @= Board^columns,
        foreach(Column in Columns, alldifferent(Column)).


sudoku(N, Board) :-
	N2 is N*N,

        array_to_list(Board,BoardVar),
        BoardVar :: 1..N2,

        alldifferent_matrix(Board),
	foreach(I in 1..N..N2, J in 1..N..N2, 
                [SubSquare],
                (SubSquare @= [Board[I+K,J+L] : 
                              K in 0..N-1, L in 0..N-1],
                 alldifferent(SubSquare))
	),


	labeling([ff,down], BoardVar).



print_board(Board) :-
        N @= Board^length,
	foreach(I in 1..N, 
                (foreach(J in 1..N, [X],
                         (X @= Board[I,J],
                          (var(X) -> write('  _') ; format('  ~q', [X]))
                         )),
                 nl)),
        nl.


%----------------------------------------------------------------------
% Sample data
%----------------------------------------------------------------------

problem(1, [](
    [](_, _, 2, _, _, 5, _, 7, 9),
    [](1, _, 5, _, _, 3, _, _, _),
    [](_, _, _, _, _, _, 6, _, _),
    [](_, 1, _, 4, _, _, 9, _, _),
    [](_, 9, _, _, _, _, _, 8, _),
    [](_, _, 4, _, _, 9, _, 1, _),
    [](_, _, 9, _, _, _, _, _, _),
    [](_, _, _, 1, _, _, 3, _, 6),
    [](6, 8, _, 3, _, _, 4, _, _))).

problem(2, [](
    [](_, _, 3, _, _, 8, _, _, 6),
    [](_, _, _, 4, 6, _, _, _, _),
    [](_, _, _, 1, _, _, 5, 9, _),
    [](_, 9, 8, _, _, _, 6, 4, _),
    [](_, _, _, _, 7, _, _, _, _),
    [](_, 1, 7, _, _, _, 9, 5, _),
    [](_, 2, 4, _, _, 1, _, _, _),
    [](_, _, _, _, 4, 6, _, _, _),
    [](6, _, _, 5, _, _, 8, _, _))).

problem(3, [](
    [](_, _, _, 9, _, _, _, _, _),
    [](_, _, 7, _, 6, _, 5, _, _),
    [](_, _, 3, 5, _, _, _, 7, 9),
    [](4, _, 5, _, _, 9, _, _, 1),
    [](8, _, _, _, _, _, _, _, 7),
    [](1, _, _, 6, _, _, 9, _, 8),
    [](6, 4, _, _, _, 8, 7, _, _),
    [](_, _, 9, _, 1, _, 2, _, _),
    [](_, _, _, _, _, 7, _, _, _))).

problem(4, [](
    [](_, 5, _, _, _, 1, 4, _, _), 
    [](2, _, 3, _, _, _, 7, _, _), 
    [](_, 7, _, 3, _, _, 1, 8, 2), 
    [](_, _, 4, _, 5, _, _, _, 7), 
    [](_, _, _, 1, _, 3, _, _, _), 
    [](8, _, _, _, 2, _, 6, _, _), 
    [](1, 8, 5, _, _, 6, _, 9, _), 
    [](_, _, 2, _, _, _, 8, _, 3), 
    [](_, _, 6, 4, _, _, _, 7, _))).

% Problems 5-8 are harder, taken from
% http://www2.ic-net.or.jp/~takaken/auto/guest/bbs46.html
problem(5, [](
    [](_, 9, 8, _, _, _, _, _, _),
    [](_, _, _, _, 7, _, _, _, _),
    [](_, _, _, _, 1, 5, _, _, _),
    [](1, _, _, _, _, _, _, _, _),
    [](_, _, _, 2, _, _, _, _, 9),
    [](_, _, _, 9, _, 6, _, 8, 2),
    [](_, _, _, _, _, _, _, 3, _),
    [](5, _, 1, _, _, _, _, _, _),
    [](_, _, _, 4, _, _, _, 2, _))).

problem(6, [](
    [](_, _, 1, _, 2, _, 7, _, _),
    [](_, 5, _, _, _, _, _, 9, _),
    [](_, _, _, 4, _, _, _, _, _),
    [](_, 8, _, _, _, 5, _, _, _),
    [](_, 9, _, _, _, _, _, _, _),
    [](_, _, _, _, 6, _, _, _, 2),
    [](_, _, 2, _, _, _, _, _, _),
    [](_, _, 6, _, _, _, _, _, 5),
    [](_, _, _, _, _, 9, _, 8, 3))).

problem(7, [](
    [](1, _, _, _, _, _, _, _, _),
    [](_, _, 2, 7, 4, _, _, _, _),
    [](_, _, _, 5, _, _, _, _, 4),
    [](_, 3, _, _, _, _, _, _, _),
    [](7, 5, _, _, _, _, _, _, _),
    [](_, _, _, _, _, 9, 6, _, _),
    [](_, 4, _, _, _, 6, _, _, _),
    [](_, _, _, _, _, _, _, 7, 1),
    [](_, _, _, _, _, 1, _, 3, _))).

problem(8, [](
    [](1, _, 4, _, _, _, _, _, _),
    [](_, _, 2, 7, 4, _, _, _, _),
    [](_, _, _, 5, _, _, _, _, _),
    [](_, 3, _, _, _, _, _, _, _),
    [](7, 5, _, _, _, _, _, _, _),
    [](_, _, _, _, _, 9, 6, _, _),
    [](_, 4, _, _, _, 6, _, _, _),
    [](_, _, _, _, _, _, _, 7, 1),
    [](_, _, _, _, _, 1, _, 3, _))).


% BBC Focus magazine October 2005
problem(9, [](
    [](_, 6, _, 3, 2, _, _, 7, _),
    [](4, 7, _, _, _, _, _, 3, 2),
    [](_, _, _, 9, _, _, 1, 4, 6),
    [](2, 4, _, 8, _, _, _, _, _),
    [](_, _, 8, _, _, _, 2, _, 1),
    [](1, _, _, _, _, 2, _, _, _),
    [](_, _, 2, 4, 7, 6, 8, _, _),
    [](6, 8, 9, _, _, _, _, 5, 4),
    [](_, _, _, _, 8, _, _, _, _))).

problem(10, [](
    [](1, 8, 2, 7, 5, _, 3, _, 9),
    [](9, 5, 6, _, 3, _, _, 8, _),
    [](3, 4, 7, _, _, 9, _, 5, _),
    [](2, _, 3, _, 4, _, _, 9, 8),
    [](4, _, 8, 9, _, 2, 5, _, 3),
    [](5, 7, 9, 3, 6, 8, 1, 2, 4),
    [](_, 2, _, 4, 9, _, 8, 3, _),
    [](_, 3, _, _, 2, _, 9, _, 5),
    [](_, 9, _, _, _, 3, _, 1, _))).

/*
  These are from J:s sudoku.ijs
*/ 
% Roger Huis example
problem(11,[](
                 [](2,_,_,6,7,_,_,_,_),
                 [](_,_,6,_,_,_,2,_,1),
                 [](4,_,_,_,_,_,8,_,_),
                 [](5,_,_,_,_,9,3,_,_),
                 [](_,3,_,_,_,_,_,5,_),
                 [](_,_,2,8,_,_,_,_,7),
                 [](_,_,1,_,_,_,_,_,4),
                 [](7,_,8,_,_,_,6,_,_),
                 [](_,_,_,_,5,3,_,_,8))).


% This puzzle is the evil puzzle from
% Perl's Games::Sudoku examples
problem(12,[](
                 [](_,7,6,4,_,_,5,_,_),
                 [](_,_,_,_,_,5,_,_,4),
                 [](_,_,_,_,7,_,_,6,9),
                 [](5,_,_,_,_,2,_,9,_),
                 [](_,3,1,_,_,_,2,5,_),
                 [](_,6,_,5,_,_,_,_,1),
                 [](6,2,_,_,4,_,_,_,_),
                 [](8,_,_,3,_,_,_,_,_),
                 [](_,_,5,_,_,7,4,3,_))).



% From https://groups.google.com/d/topic/comp.lang.prolog/sTSzJMflBDw/discussion
problem(13,[](
                 [](_,_,_,_,_,_,_,1,2),
                 [](_,_,_,_,_,_,_,_,3),   
                 [](_,_,2,3,_,_,4,_,_),
                 [](_,_,1,8,_,_,_,_,5),
                 [](_,6,_,_,7,_,8,_,_),
                 [](_,_,_,_,_,9,_,_,_),
                 [](_,_,8,5,_,_,_,_,_),
                 [](9,_,_,_,4,_,5,_,_),
                 [](4,7,_,_,_,6,_,_,_))).
