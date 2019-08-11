/*

  KenKen puzzle in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
    problem(1,N, Problem),
    time(kenken2(N, Problem)),
    nl.




kenken2(N, Problem) :-
   
        %% decision variables
        new_matrix(N,N,1..N,X),

        %% all rows and columns must be unique
        latin_square(X),
        %% Check the hints
        maplist(check_hint(X),Problem),

        flatten(X,Vars),
        labeling([],Vars),

        print_matrix(X).

%% The hint constraints
check_hint(X,[Result,Coeffs]) :-
        calc(Result,Coeffs,X).

calc(Result, Coeffs,X) :-
        length(Coeffs,Len),        
        (Len == 2
        ->
         %% size 2
         [[AR,AC],[BR,BC]] = Coeffs,
         matrix_element(X,AR,AC,A), % A #= X[AR,AC],
         matrix_element(X,BR,BC,B), % B #= X[BR,BC],
         (
          A + B #= Result #\/
         A * B #= Result #\/
         A * Result #= B #\/    % B/A = Result
         B * Result #= A #\/    % A/B = Result
         A - B #= Result #\/
         B - A #= Result
         )
        ;
         %% or size > 2
         extract_from_indices2d(Coeffs,X,Coeffs2),
         check_many(Result, Coeffs2)
        ).


% either sum or product
check_many(Result, CoeffRes) :-
        prodlist(CoeffRes,Result).
check_many(Result, CoeffRes) :-
        sum(CoeffRes,#=,Result).

% product of a list
mult(X,Y,Z) :- Z #= X*Y. % helper predicate
prodlist(List,Product) :-
        foldl(mult,List,1,Product).

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
problem(1, Size, M) :- 
        Size = 6,
        M = [
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
            ].


