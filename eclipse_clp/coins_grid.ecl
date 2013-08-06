/*

  Coins puzzle in MiniZinc.
 
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
 

  This CP model is very slow. It should really be integer programming.

  Compare with these models:
  * MiniZinc: http://www.hakank.org/minizinc/coins_grid.mzn
              (note: with a linear programming solver this is very fast)
  * Choco   : http://www.hakank.org/choco/CoinsGrid.java
  * JaCoP   : http://www.hakank.org/JaCoP/CoinsGrid.java
  * Gecode/R: http://www.hakank.org/gecode_r/coins_grid.rb
  * Comet   : http://www.hakank.org/comet/coins_grid.co
              (this is a integer programming model)            
  * Gecode  : http://www.hakank.org/gecode/coins_grid.cpp

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_search).
:-lib(ic_global).
:-lib(branch_and_bound).


% TODO: also make an eplex version!


pretty_print(X) :-
        dim(X, [N,N]),
        % concat_string(["%", N, "%"], Format),
        (
            for(I, 1, N), 
            param(X, N) 
        do
            (
                for(J, 1, N),
                param(X, I)
            do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        
        ).        

go :-
        N = 10, % 31 the grid size
        C = 3,  % 14, number of coins per row/column
        
        dim(X, [N,N]),
        X[1..N,1..N] :: 0..1,
        % Sum :: 0..99999,
        Sum #>= 0,

        ( % sum of rows and column = C
            for(I, 1, N), 
            param(X,N,C) do
                C #= sum(X[I,1..N]), % rows
                C #= sum(X[1..N,I])  % columns    
        ),

        % quadratic horizontal distance
        (
            for(I, 1, N) * for(J, 1, N),  
            fromto(0, In, Out, Sum),
            param(X) 
        do
            Out #= In + (X[I,J] * abs(I-J)*abs(I-J))
        ),

        term_variables(X, Vars),
        minimize(search(Vars, 0,occurrence,indomain_min,complete,[]),Sum),
        % search(Vars, 0,first_fail,indomain,complete,[]),
        writeln(sum:Sum),
        pretty_print(X).
                