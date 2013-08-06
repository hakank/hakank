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
 

  This is an eplex (integer programming) solution which is much 
  faster than the constraint programming solution in coins_grid.ecl.

  Compare with these models, mostly CP approaches:
  * MiniZinc: http://www.hakank.org/minizinc/coins_grid.mzn (CP|MIP)
              with a linear programming solver this is very fast.
  * Choco   : http://www.hakank.org/choco/CoinsGrid.java  (CP)
  * JaCoP   : http://www.hakank.org/JaCoP/CoinsGrid.java (CP)
  * Gecode/R: http://www.hakank.org/gecode_r/coins_grid.rb  (CP)
  * Comet   : http://www.hakank.org/comet/coins_grid.co (MIP)
              (this is a integer programming model)            
  * Gecode  : http://www.hakank.org/gecode/coins_grid.cpp (CP)
  * ECLiPSe : http://www.hakank.org/ECLiPSe/coins_grid.ecl (CP)



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(eplex).


pretty_print(X) :-
        dim(X, [N,N]),
        ( for(I, 1, N), param(X, N) do
            ( for(J, 1, N), param(X, I) do
                XIJ is floor(X[I,J]),
                write(XIJ), write(" ")
            ),
            nl
        ).        

go :-
        eplex_solver_setup(min(Sum)),
        N = 31, % 31, % the grid size
        C = 14, % 14, % number of coins per row/column
        
        dim(X, [N,N]),

        Sum $>= 0,
        (
            for(I, 1, N), 
            param(X,N,C) do
                C $= sum(X[I,1..N]), % rows
                C $= sum(X[1..N,I])  % columns    
        ),

        %
        % calculate the quadratic horizontal distance
        % 
        (
            for(I, 1, N) * for(J, 1, N),  
            fromto(0, In, Out, Sum),
            param(X) 
        do
            % make X[I,J] just 0..1 (and integers)
            integers([X[I,J]]),
            X[I,J] :: 0.0..1.0,
            Out $= In + X[I,J] * abs(I-J)*abs(I-J)
        ),

        eplex_solve(Sum),
        eplex_get(vars, Vars),
        eplex_get(typed_solution, Val),
        Vars = Val, 
        nl,
        pretty_print(X),
        fix(Sum,FixedSum),
        writeln(sum:FixedSum).
