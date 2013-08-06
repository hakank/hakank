/*

  KenKen puzzle in B-Prolog.

  http://en.wikipedia.org/wiki/KenKen
  """
  KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing 
  several characteristics with sudoku. The name comes from Japanese and 
  is translated as "square wisdom" or "cleverness squared".
  ...
  The objective is to fill the grid in with the digits 1 through 6 such that:

    * Each row contains exactly one of each digit
    * Each column contains exactly one of each digit
    * Each bold-outlined group of cells is a cage containing digits which 
      achieve the specified result using the specified mathematical operation: 
        addition (+), 
        subtraction (-), 
        multiplication (x), 
        and division (Ã·). 
        (Unlike in Killer sudoku, digits may repeat within a group.)

  ...
  More complex KenKen problems are formed using the principles described 
  above but omitting the symbols +, -, x and Ã·, thus leaving them as 
  yet another unknown to be determined.
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


go :-
        problem(1,N, Problem),
        time2(kenken(N, Problem)).
        

%
% Benchmark all heuristics
%
go2 :-
        selection(VarSelection),
        choice(ValSelection),
        problem(1,N,Problem),
        foreach(Var in VarSelection, Val in ValSelection,
                (writeln([Var,Val]),
                 time2(kenken2(N,Problem,Var,Val)),
                 nl,nl
                )
               ).

%
% Ensure a Latin square, 
% i.e. all rows and all columns are different
%
alldifferent_matrix(Board) :-
        Rows @= Board^rows,
        foreach(Row in Rows, alldistinct(Row)),
        Columns @= Board^columns,
        foreach(Column in Columns, alldistinct(Column)).


kenken(N,Problem) :-
        kenken2(N, Problem,ff,updown).


kenken2(N, Problem,VarSelect,ValSelect) :-

        length(Problem,Len),

        % decision variables
        new_array(X,[N,N]),
        array_to_list(X, Vars),
        Vars :: 1..N,

        % all rows and columns must be unique
        alldifferent_matrix(X),

        foreach(I in 1..Len, [Hint,Result,Coeffs],
                (
                    % extract the hints
                    Hint @= Problem[I],
                    [Result, Coeffs] = Hint,
                    
                    % calculate this hint
                    calc(Result, Coeffs,X)
                )
        ),

        labeling([VarSelect,ValSelect],Vars),
        pretty_print(X).



calc(Result, Coeffs,X) :-
        length(Coeffs, Len),
        (
            % special handling for size 2
            Len == 2,
            [[AR,AC],[BR,BC]] = Coeffs,
            A #= X[AR,AC],
            B #= X[BR,BC],
            check2(A,B,Result)
        )
        ;
        (
            % or size > 2
            CoeffRes @= [X[R,C] : [R,C] in Coeffs],
            check_many(Result, CoeffRes)
        ).


%
% all alternatives for 2 argument 
% I assume that only segments with 2 cells can be 
% minus or div.
% 
check2(A,B,Result) :- A + B #= Result.  
check2(A,B,Result) :- A * B #= Result.
check2(A,B,Result) :- A * Result #= B. % B/A = Result
check2(A,B,Result) :- B * Result #= A. % A/B = Result
check2(A,B,Result) :- A - B #= Result.
check2(A,B,Result) :- B - A #= Result.

% either sum or product
check_many(Result, CoeffRes) :- prod(CoeffRes,Result).
check_many(Result, CoeffRes) :- Result #= sum(CoeffRes).


% product of a list
prod(List, Product) :-
        % This don't work: I.e. to have the Product directly as a accumulator
        % foreach(L in List, ac(Product,1), Product^1 #= Product^0*L),
        % Instead, we must use a temporary ac.
        foreach(L in List, ac(Product1,1), Product1^1 #= Product1^0*L),
        Product = Product1.


pretty_print(Board) :-
        N @= Board^length,
	foreach(I in 1..N,
                (foreach(J in 1..N, [X],
                         (X @= Board[I,J],
                          (var(X) -> write('  _') ; format(' ~q', [X]))
                         )
                        ),
                 nl
                )
               ),
        nl.

%
% State the problem, i.e. the hints. 
%
% For a better view of the problem, see 
%  http://en.wikipedia.org/wiki/File:KenKenProblem.svg  
%
%
%   The solution is:
%     5 6 3 4 1 2
%     6 1 4 5 2 3
%     4 5 2 3 6 1
%     3 4 1 2 5 6
%     2 3 6 1 4 5
%     1 2 5 6 3 4
%
problem(1, 6, [
               [ 11, [[1,1], [2,1]]],
               [  2, [[1,2], [1,3]]],
               [ 20, [[1,4], [2,4]]],
               [  6, [[1,5], [1,6], [2,6], [3,6]]],
               [  3, [[2,2], [2,3]]],
               [  3, [[2,5], [3,5]]],
               [240, [[3,1], [3,2], [4,1], [4,2]]],
               [  6, [[3,3], [3,4]]],  
               [  6, [[4,3], [5,3]]],
               [  7, [[4,4], [5,4], [5,5]]],
               [ 30, [[4,5], [4,6]]],  
               [  6, [[5,1], [5,2]]],
               [  9, [[5,6], [6,6]]],
               [  8, [[6,1], [6,2], [6,3]]],
               [  2, [[6,4], [6,5]]]
           ]).


% Variable selection
selection([backward,constr,degree,ff,ffc,forward,inout,leftmost,max,min]).

% Value selection
choice([down,updown,split,reverse_split]).
