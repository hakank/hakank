/*

  Coins grid problem in AMPL.

  Problem from 
  Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one should place coins in such a
  way that the following conditions are fulfilled:
     1. In each row exactly 14 coins must be placed.
     2. In each column exactly 14 coins must be placed.
     3. The sum of the quadratic horizontal distance from the main diagonal of all cells
        containing a coin must be as small as possible.
     4. In each cell at most one coin can be placed.
  The description says to place 14x31 = 434 coins on the chessboard each row containing 14
  coins and each column also containing 14 coins.
  """

  (This is a standard problem when I test CP solvers, though MIP solvers tend to be faster.)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param c;

var x{1..n, 1..n} binary;
var z = sum{i in 1..n, j in 1..n} (x[i,j]*(abs(i-j)*abs(i-j)));

minimize obj: z;

s.t. c1{i in 1..n}: sum{j in 1..n} (x[i,j]) = c;
s.t. c2{j in 1..n}: sum{i in 1..n} (x[i,j]) = c;

data;

# original problem
# let n := 31;
# let c := 14;

let n := 15; # 31;
let c := 4;  # 14;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=10";
# option solver cplex;

solve;


display n,c;
display x;
display z;