/*

  Sudoku using global cardinality in ECLiPSe.

  The model is based of the example from: 

     http://www.eclipse-clp.org/eclipse/examples/sudoku.ecl.txt

  where the ic_global:alldifferent is compared to my decompositions of
  global_cardinality and alldifferent. The latter are much slower, as
  expected.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:- lib(ic).
:- lib(ic_global).
:- lib(ic_search).
:- lib(listut).
% :- lib(propia).


%
% This version of global cardinality is bidirectional but limited:
%
% The array A can contain only values in the range 1..Max (i.e. the 
% length of Gcc).
%
% This means that the caller must know the maximum values of A.
% Or rather: if A contains a value outside this range it will not be counted.
% 
% And, of course, it is very costly.
%
global_cardinality(A, Gcc) :-
        dim(Gcc,[Max]),
        (for(I,1,Max), param(Gcc,A) do
             listut:nth1(I,A,II),
             ic_global:occurrences(II,A,C),
             Gcc[I] #= C
        ).


%
% Just for fun, here is a simple decomposition of alldifferent.
%
% This seems to have exactly the same number of backtracks as
% global_cardinality.
%
alldifferent_decomp(Xs) :-
        length(Xs,Len),
        dim(A, [Len]),
        (for(I,1,Len), foreach(X, Xs), param(A) do
             A[I] #= X
        ),
        ( for(I,1,Len) * for(J,1,Len), param(A) do 
              I \= J
        ->
          A[I] #\= A[J]
        ; 
          true
        ).


%
% Test all 13 problems and compare the backtracks.
%
go :-
        ( for(P,1,13) do
              solve_all(P,Backtracks),
              writeln([problem:P,backtracks:Backtracks])
        ).


solve_all(Problem,[B1,B2,B3]) :-
        solve(Problem,alldifferent, B1),
        solve(Problem,gcc,B2),        
        solve(Problem,alldifferent_decomp,B3).

%
% solve a problem instance with a specific constraint (Type)
%
solve(ProblemName,Type,Backtracks) :-
	problem(ProblemName, Board),
	sudoku(3, Board,Type,Backtracks).


%
% Note: Most of this code in sudoku/4 is from
%  http://www.eclipse-clp.org/eclipse/examples/sudoku.ecl.txt
%
% I have just added Backtracks and the type of constraint to use.
%
sudoku(N, Board, Type, Backtracks) :-
	N2 is N*N,
        Gcc = [](1,1,1,1,1,1,1,1,1), % global cardinality count
	dim(Board, [N2,N2]),
	Board[1..N2,1..N2] :: 1..N2,
	( for(I,1,N2), param(Board,N2,Gcc,Type) do
	    Row is Board[I,1..N2],
	    Col is Board[1..N2,I],
            (
            Type == alldifferent ->
                ic_global:alldifferent(Row),
                ic_global:alldifferent(Col)
            ;
                Type == gcc -> 
                    global_cardinality(Row,Gcc),
                    global_cardinality(Col,Gcc)
            ;
                    alldifferent_decomp(Row),
                    alldifferent_decomp(Col)
            )
	),
	( multifor([I,J],1,N2,N), param(Board,N,Gcc,Type) do
	    ( multifor([K,L],0,N-1), param(Board,I,J), foreach(X,SubSquare) do
		X is Board[I+K,J+L]
	    ),
            (
                Type == alldifferent 
            ->
                ic_global:alldifferent(SubSquare)
            ; 
                Type == gcc 
            ->
                global_cardinality(SubSquare, Gcc)
            ; 
                  alldifferent_decomp(SubSquare)
            )
        ),

	term_variables(Board, Vars),
        search(Vars,0,first_fail,indomain,complete,[backtrack(Backtracks)]).



print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
	    	X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.


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

% this one is from http://www.skyone.co.uk/programme/pgefeature.aspx?pid=48&fid=129
problem(9, [](
    [](5, _, 6, _, 2, _, 9, _, 3),
    [](_, _, 8, _, _, _, 5, _, _),
    [](_, _, _, _, _, _, _, _, _),
    [](6, _, _, 2, 8, 5, _, _, 9),
    [](_, _, _, 9, _, 3, _, _, _),
    [](8, _, _, 7, 6, 1, _, _, 4),
    [](_, _, _, _, _, _, _, _, _),
    [](_, _, 4, _, _, _, 3, _, _),
    [](2, _, 1, _, 5, _, 6, _, 7))).

% BBC Focus magazine October 2005
problem(10, [](
    [](_, 6, _, 3, 2, _, _, 7, _),
    [](4, 7, _, _, _, _, _, 3, 2),
    [](_, _, _, 9, _, _, 1, 4, 6),
    [](2, 4, _, 8, _, _, _, _, _),
    [](_, _, 8, _, _, _, 2, _, 1),
    [](1, _, _, _, _, 2, _, _, _),
    [](_, _, 2, 4, 7, 6, 8, _, _),
    [](6, 8, 9, _, _, _, _, 5, 4),
    [](_, _, _, _, 8, _, _, _, _))).

problem(11, [](
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
  This two are from ~/j/me/sudoku.ijs
*/ 
% Roger Hui's example
problem(12,[](
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
% Perl module Games::Sudoku
problem(13,[](
                 [](_,7,6,4,_,_,5,_,_),
                 [](_,_,_,_,_,5,_,_,4),
                 [](_,_,_,_,7,_,_,6,9),
                 [](5,_,_,_,_,2,_,9,_),
                 [](_,3,1,_,_,_,2,5,_),
                 [](_,6,_,5,_,_,_,_,1),
                 [](6,2,_,_,4,_,_,_,_),
                 [](8,_,_,3,_,_,_,_,_),
                 [](_,_,5,_,_,7,4,3,_))).


