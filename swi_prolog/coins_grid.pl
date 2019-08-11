/*

  Coins grid puzzle in SWI Prolog

  Problem from 
  Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one 
  should place coins in such a way that the following conditions are 
  fulfilled:
    1. In each row exactly 14 coins must be placed.
    2. In each column exactly 14 coins must be placed.
    3. The sum of the quadratic horizontal distance from the main
       diagonal of all cells containing a coin must be as small as possible.
    4. In each cell at most one coin can be placed.

   The description says to place 14x31 = 434 coins on the chessboard 
   each row containing 14 coins and each column also containing 14 coins.
  """

  Note: This problem is quite hard for most C(L)P solvers.
  A MIP solver solves the full 14,31 problem in millis.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   N = 7, % 31,
   C = 3, % 14,
   time(once(coins(N, C))).


%%
%% standard CLP(FD) approach
%%
coins(N,C) :-
   
   new_matrix(N,N,0..1, X),

   %% quadratic horizontal distance
   total_sum(X, N, N, Sum),

   %% Rows and columns sums to C,
   sums(X, C),
   transpose(X, Transposed),
   sums(Transposed, C),

   flatten(X, Vars),
   labeling([min(Sum),down, bisect],Vars),
   writeln(sum=Sum),
   print_matrix(X).

%%
%% Total sum of X[I,J]'s
%%
total_sum(X, Rows, Cols, Sum) :-
        findall([I,J], (between(1,Rows,I), between(1,Cols,J)), Ixes),
        total_sum_(Ixes, X, 0, Sum).

total_sum_([], _X, Sum0, Sum) :-
        Sum0 #= Sum.
total_sum_([[I,J]|IJs],X, Sum0, TotalSum) :-
        matrix_element(X,I,J, Val),
        A #= abs(I-J),
        Sum1 #= (Val*A*A)+Sum0,
        total_sum_(IJs,X,Sum1, TotalSum).

% sum of the rows and cols are #= Sum 
sums([], _Sum).
sums([L|Ls], Sum) :-
        sum(L, #=, Sum),
        sums(Ls, Sum).


