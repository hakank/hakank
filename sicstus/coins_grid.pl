/*

  Coins puzzle in SICStus Prolog.

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
  Cf the LPL model:
  http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
 

  This CP model is very slow (for all CP solvers). It should really 
  be integer programming.

  Compare with these models:
  * MiniZinc: http://www.hakank.org/minizinc/coins_grid.mzn
              (note: with a linear programming solver this is very fast)
  * Choco   : http://www.hakank.org/choco/CoinsGrid.java
  * JaCoP   : http://www.hakank.org/JaCoP/CoinsGrid.java
  * Gecode/R: http://www.hakank.org/gecode_r/coins_grid.rb
  * Comet   : http://www.hakank.org/comet/coins_grid.co
              (this is a integer programming model)            
  * Gecode  : http://www.hakank.org/gecode/coins_grid.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/coins_grid.ecl
  * ECLiPSe : http://www.hakank.org/eclipse/coins_grid_eplex.ecl (MIP)

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).



go :-
        N = 10, % 31 the grid size
        C = 3,  % 14, number of coins per row/column
        
        matrix(X, [N,N]),
        append(X, Vars),
        domain(Vars, 0, 1),

        % sum of rows and column = C
        ( 
            foreach(Row, X),
            param(C)
        do
            sum(Row, #=, C)
        ),

        transpose(X, Columns),
        ( 
            foreach(Column, Columns),
            param(C)
        do
            sum(Column, #=, C)
        ),

        

        % quadratic horizontal distance
        (
            for(I, 1, N),
            fromto(0, In, Out, Sum),
            param(X,N)
        do 
            (
                for(J, 1, N),  
                fromto(0, InJ, OutJ, SumJ),
                param(X,I) 
            do
                nth1(I, X, Row),
                element(J, Row, Element),
                domain([Element],0,1),
                OutJ #= InJ + (Element * abs(I-J)*abs(I-J))
            ),
            Out #= In + SumJ
        ),

        Sum #>= 0,

        labeling([ff,bisect,down,minimize(Sum)], Vars),
        write(sum:Sum),nl,
        pretty_print(X),
        fd_statistics.
                

pretty_print(X) :-
        (
          foreach(Row, X)
        do
          write(Row),nl
        ).        

% Suggested by Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
