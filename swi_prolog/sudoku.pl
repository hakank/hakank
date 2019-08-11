/*

  Sudoku in SWI Prolog

  Some of the problem instances (and some idea) are from 
  ECLiPSe's Sudoku model: 
       http://eclipseclp.org/examples/sudoku.ecl.txt

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(library(readutil)).
:- use_module(hakank_utils).


%%
%% Solve the first problem.
%% Ensure unicity with findall/3.
%%
go :- 
        time(findall(_, sudoku(1),_)),
        nl.


%%
%% Solve the hardest problem (#13)
%%
go2 :-
        between(1,15,P),
        writeln(problem=P),
        time(sudoku(P)),
        nl,
        fail,
        nl.

go2.

%%
%% Problem 13 is the hardest problem.
%% We benchmark this problem to get the fastest labelings.
%%
%% It seems that (ff|ffc + up + step|enum) are the best choices.
%%
% [leftmost,up,step], Time: 1.466181s
% [leftmost,up,enum], Time: 1.434018s
% [leftmost,up,bisect], Time: 1.491451s
% [leftmost,down,step], Time: 0.172750s
% [leftmost,down,enum], Time: 0.165210s
% [leftmost,down,bisect], Time: 0.171285s
% [ff,up,step], Time: 0.082918s <--
% [ff,up,enum], Time: 0.083588s <--
% [ff,up,bisect], Time: 0.083105s
% [ff,down,step], Time: 0.215247s
% [ff,down,enum], Time: 0.214360s
% [ff,down,bisect], Time: 0.216822s
% [ffc,up,step], Time: 0.083776s  <--
% [ffc,up,enum], Time: 0.083808s  <--
% [ffc,up,bisect], Time: 0.083925s
% [ffc,down,step], Time: 0.221777s
% [ffc,down,enum], Time: 0.219717s
% [ffc,down,bisect], Time: 0.222002s
% [min,up,step], Time: 0.903215s
% [min,up,enum], Time: 0.918933s
% [min,up,bisect], Time: 1.050274s
% [min,down,step], Time: 2.439207s
% [min,down,enum], Time: 2.180898s
% [min,down,bisect], Time: 1.944945s
% [max,up,step], Time: 0.428134s
% [max,up,enum], Time: 0.400883s
% [max,up,bisect], Time: 1.239747s
% [max,down,step], Time: 0.547487s
% [max,down,enum], Time: 1.416076s
% [max,down,bisect], Time: 0.909731s
%
go3 :-
        VarSelect = [leftmost,ff,ffc,min,max],
        ValSelect = [up,down],
        BranchSelect = [step,enum,bisect],
        member(Var,VarSelect),
        member(Val,ValSelect),
        member(Branch, BranchSelect),
        Label = [Var,Val,Branch],
        problem(13,X),
        time2(sudoku_with_label(3,X,Label),Time),
        format("~w, Time: ~fs~n", [Label, Time]),
        fail,
        nl.

go3.



%%
%% Solving Norvig's 95 Sudoku's
%% http://norvig.com/top95.txt
%%
%% It takes about 9.7s to solve all 95 Sudokus.
%%
go4 :-
        File = 'top95.txt',        
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "",Lines),
        %% First line
        %% X = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
        length(Lines, NumProblems),
        writeln(numProblems=NumProblems),
        member(Line,Lines),
        string_chars(Line,Chars),
        %% Slice and convert the string line into a Sudoku instance (9 x 9 matrix).
        findall(T,
                (
                 between(0,8,I),
                 Start #= 1+I*9-1,
                 End #= 9+I*9-1,
                 slice(Chars, Start,End, S),
                 convert_chars(S,[],T)
                 ),
                Board
               ),
        print_board(Board),
        time2(sudoku(3,Board),Time),
        print_board(Board),
        format("Time ~f~n", [Time]),
        nl,
        fail,
        nl.

go4. 


%%
%% Solve Sudoku problem P
%%
sudoku(P) :-
	problem(P, X),
	print_board(X),
	sudoku(3, X),
	print_board(X).

%%
%% sudoku(N, X)
%%
%% Solve a Sudoku for the board/matrix X with a cell size of N (3).
%%
sudoku(N, X) :-
        N2 #= N*N,
        
        flatten(X, Vars),
        Vars ins 1..N2,
        
        %% latin_square
        maplist(all_distinct, X),
        transpose(X,XT),
        maplist(all_distinct, XT),

        %% The cells
        findall([I,J], (between(1,N,N2,I),
                        between(1,N,N2,J)),
                IJs),
        cells(IJs,N, X),
        labeling([ffc,up,step], Vars).


%%
%% With labeling (for go3/0).
%%
sudoku_with_label(N, X,Label) :-
        N2 #= N*N,
        
        flatten(X, Vars),
        Vars ins 1..N2,

        %% latin square
        maplist(all_distinct, X),
        transpose(X,XT),
        maplist(all_distinct, XT),

        %% The cells
        findall([I,J], (between(1,N,N2,I),
                        between(1,N,N2,J)),
                IJs),
        cells(IJs,N, X),
        
        labeling(Label, Vars).


cells([],_N, _X).
cells([[I,J]|IJs], N, X) :-
        N1 #= N-1,
        % Don't work.
        % findall(XIAJB, (
        %                 between(0,N1,A),
        %                 between(0,N1,B),
        %                 IA #= I+A, JB #= J+B,
        %                 matrix_element(X, IA, JB, XIAJB)),
        %         Cells),
        findall([IA,JB],
                (
                 between(0,N1,A),
                 between(0,N1,B),
                 IA #= I+A, JB #= J+B
                 ),
                Cells),
        cells_(Cells, X, [], Xs),
        all_distinct(Xs),
        cells(IJs, N, X).

cells_([], _X, Row, Row).
cells_([[I,J]|IJs], X, Row0, Row) :-
        matrix_element(X,I,J,XIJ),
        cells_(IJs, X, [XIJ|Row0], Row).
        


%%
%% Pretty print the matrix.
%%
print_board(X) :-
        maplist(print_board_element,X),
        nl.

print_board_element([]) :- nl.
print_board_element([E|Row]) :-
        (
         nonvar(E)
        ->
         write(E)
        ;
         write("_")
        ),
        write(" "),
        print_board_element(Row).


%%
%% Convert a line of "...4...81" to a board line [_,_,_,4._,_,8,1]
%%
%% For go4/0 (Norvig's Sudokus)
%%
convert_chars([],Chars,Chars).
convert_chars([C|Cs], Chars0,[T|Chars]) :-
         (C = '.'
         ->
          T = _
         ;
          atom_number(C,T)
         ),
         convert_chars(Cs,Chars0,Chars).




%----------------------------------------------------------------------
% Sample data
%----------------------------------------------------------------------

%% From http://eclipseclp.org/examples/sudoku.ecl.txt
problem(1, Data) :- 
Data = [
    [_, _, 2, _, _, 5, _, 7, 9],
    [1, _, 5, _, _, 3, _, _, _],
    [_, _, _, _, _, _, 6, _, _],
    [_, 1, _, 4, _, _, 9, _, _],
    [_, 9, _, _, _, _, _, 8, _],
    [_, _, 4, _, _, 9, _, 1, _],
    [_, _, 9, _, _, _, _, _, _],
    [_, _, _, 1, _, _, 3, _, 6],
    [6, 8, _, 3, _, _, 4, _, _]].

problem(2, Data) :- 
 Data = [
    [_, _, 3, _, _, 8, _, _, 6],
    [_, _, _, 4, 6, _, _, _, _],
    [_, _, _, 1, _, _, 5, 9, _],
    [_, 9, 8, _, _, _, 6, 4, _],
    [_, _, _, _, 7, _, _, _, _],
    [_, 1, 7, _, _, _, 9, 5, _],
    [_, 2, 4, _, _, 1, _, _, _],
    [_, _, _, _, 4, 6, _, _, _],
    [6, _, _, 5, _, _, 8, _, _]].

problem(3, Data) :- 
Data = [
    [_, _, _, 9, _, _, _, _, _],
    [_, _, 7, _, 6, _, 5, _, _],
    [_, _, 3, 5, _, _, _, 7, 9],
    [4, _, 5, _, _, 9, _, _, 1],
    [8, _, _, _, _, _, _, _, 7],
    [1, _, _, 6, _, _, 9, _, 8],
    [6, 4, _, _, _, 8, 7, _, _],
    [_, _, 9, _, 1, _, 2, _, _],
    [_, _, _, _, _, 7, _, _, _]].

problem(4, Data) :- 
Data = [
    [_, 5, _, _, _, 1, 4, _, _], 
    [2, _, 3, _, _, _, 7, _, _], 
    [_, 7, _, 3, _, _, 1, 8, 2], 
    [_, _, 4, _, 5, _, _, _, 7], 
    [_, _, _, 1, _, 3, _, _, _], 
    [8, _, _, _, 2, _, 6, _, _], 
    [1, 8, 5, _, _, 6, _, 9, _], 
    [_, _, 2, _, _, _, 8, _, 3], 
    [_, _, 6, 4, _, _, _, 7, _]].

% Problems 5-8 are harder, taken from
% http://www2.ic-net.or.jp/~takaken/auto/guest/bbs46.html
problem(5, Data) :- Data = [
    [_, 9, 8, _, _, _, _, _, _],
    [_, _, _, _, 7, _, _, _, _],
    [_, _, _, _, 1, 5, _, _, _],
    [1, _, _, _, _, _, _, _, _],
    [_, _, _, 2, _, _, _, _, 9],
    [_, _, _, 9, _, 6, _, 8, 2],
    [_, _, _, _, _, _, _, 3, _],
    [5, _, 1, _, _, _, _, _, _],
    [_, _, _, 4, _, _, _, 2, _]].

problem(6, Data) :- 
Data = [
    [_, _, 1, _, 2, _, 7, _, _],
    [_, 5, _, _, _, _, _, 9, _],
    [_, _, _, 4, _, _, _, _, _],
    [_, 8, _, _, _, 5, _, _, _],
    [_, 9, _, _, _, _, _, _, _],
    [_, _, _, _, 6, _, _, _, 2],
    [_, _, 2, _, _, _, _, _, _],
    [_, _, 6, _, _, _, _, _, 5],
    [_, _, _, _, _, 9, _, 8, 3]].

problem(7, Data) :- 
Data = [
    [1, _, _, _, _, _, _, _, _],
    [_, _, 2, 7, 4, _, _, _, _],
    [_, _, _, 5, _, _, _, _, 4],
    [_, 3, _, _, _, _, _, _, _],
    [7, 5, _, _, _, _, _, _, _],
    [_, _, _, _, _, 9, 6, _, _],
    [_, 4, _, _, _, 6, _, _, _],
    [_, _, _, _, _, _, _, 7, 1],
    [_, _, _, _, _, 1, _, 3, _]].

problem(8, Data) :- 
Data = [
    [1, _, 4, _, _, _, _, _, _],
    [_, _, 2, 7, 4, _, _, _, _],
    [_, _, _, 5, _, _, _, _, _],
    [_, 3, _, _, _, _, _, _, _],
    [7, 5, _, _, _, _, _, _, _],
    [_, _, _, _, _, 9, 6, _, _],
    [_, 4, _, _, _, 6, _, _, _],
    [_, _, _, _, _, _, _, 7, 1],
    [_, _, _, _, _, 1, _, 3, _]].


% BBC Focus magazine October 2005
problem(9, Data) :- 
Data = [
    [_, 6, _, 3, 2, _, _, 7, _],
    [4, 7, _, _, _, _, _, 3, 2],
    [_, _, _, 9, _, _, 1, 4, 6],
    [2, 4, _, 8, _, _, _, _, _],
    [_, _, 8, _, _, _, 2, _, 1],
    [1, _, _, _, _, 2, _, _, _],
    [_, _, 2, 4, 7, 6, 8, _, _],
    [6, 8, 9, _, _, _, _, 5, 4],
    [_, _, _, _, 8, _, _, _, _]].

problem(10, Data) :- 
Data = [
    [1, 8, 2, 7, 5, _, 3, _, 9],
    [9, 5, 6, _, 3, _, _, 8, _],
    [3, 4, 7, _, _, 9, _, 5, _],
    [2, _, 3, _, 4, _, _, 9, 8],
    [4, _, 8, 9, _, 2, 5, _, 3],
    [5, 7, 9, 3, 6, 8, 1, 2, 4],
    [_, 2, _, 4, 9, _, 8, 3, _],
    [_, 3, _, _, 2, _, 9, _, 5],
    [_, 9, _, _, _, 3, _, 1, _]].

/*
  These are from J:s sudoku.ijs
*/ 
% Roger Huis example
problem(11,Data) :- 
Data = [
       [2,_,_,6,7,_,_,_,_],
       [_,_,6,_,_,_,2,_,1],
       [4,_,_,_,_,_,8,_,_],
       [5,_,_,_,_,9,3,_,_],
       [_,3,_,_,_,_,_,5,_],
       [_,_,2,8,_,_,_,_,7],
       [_,_,1,_,_,_,_,_,4],
       [7,_,8,_,_,_,6,_,_],
       [_,_,_,_,5,3,_,_,8]].


% This puzzle is the evil puzzle from
% Perl's Games::Sudoku examples
problem(12, Data) :- 
Data = [
       [_,7,6,4,_,_,5,_,_],
       [_,_,_,_,_,5,_,_,4],
       [_,_,_,_,7,_,_,6,9],
       [5,_,_,_,_,2,_,9,_],
       [_,3,1,_,_,_,2,5,_],
       [_,6,_,5,_,_,_,_,1],
       [6,2,_,_,4,_,_,_,_],
       [8,_,_,3,_,_,_,_,_],
       [_,_,5,_,_,7,4,3,_]].



% From https://groups.google.com/d/topic/comp.lang.prolog/sTSzJMflBDw/discussion
problem(13, Data) :- 
Data = [
       [_,_,_,_,_,_,_,1,2],
       [_,_,_,_,_,_,_,_,3],   
       [_,_,2,3,_,_,4,_,_],
       [_,_,1,8,_,_,_,_,5],
       [_,6,_,_,7,_,8,_,_],
       [_,_,_,_,_,9,_,_,_],
       [_,_,8,5,_,_,_,_,_],
       [9,_,_,_,4,_,5,_,_],
       [4,7,_,_,_,6,_,_,_]].

% First problem from Project Euler #96:
% http://projecteuler.net/problem=96
problem(14,Data) :- 
Data = 
[
[_,_,3,_,2,_,6,_,_],
[9,_,_,3,_,5,_,_,1],
[_,_,1,8,_,6,4,_,_],
[_,_,8,1,_,2,9,_,_],
[7,_,_,_,_,_,_,_,8],
[_,_,6,7,_,8,2,_,_],
[_,_,2,6,_,9,5,_,_],
[8,_,_,2,_,3,_,_,9],
[_,_,5,_,1,_,3,_,_]
].


% http://blag.nullteilerfrei.de/2014/07/03/why-someone-thought-that-sudoku-might-not-be-boring-while-actually-you-should-learn-how-to-properly-implement-backtracking/
problem(15, Data) :-
  Data =
 [
  [_, _, _, _, 6, _, _, 8, _],
  [_, 2, _, _, _, _, _, _, _],
  [_, _, 1, _, _, _, _, _, _],
  [_, 7, _, _, _, _, 1, _, 2],
  [5, _, _, _, 3, _, _, _, _],
  [_, _, _, _, _, _, 4, _, _],
  [_, _, 4, 2, _, 1, _, _, _],
  [3, _, _, 7, _, _, 6, _, _],
  [_, _, _, _, _, _, _, 5, _] 
].
