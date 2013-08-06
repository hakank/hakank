%
% Coins puzzle in MiniZinc.
% 
% Problem from 
% Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
% http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
%
% """
% In a quadratic grid (or a larger chessboard) with 31x31 cells, one should place coins in such a
% way that the following conditions are fulfilled:
%    1. In each row exactly 14 coins must be placed.
%    2. In each column exactly 14 coins must be placed.
%    3. The sum of the quadratic horizontal distance from the main diagonal of all cells
%       containing a coin must be as small as possible.
%    4. In each cell at most one coin can be placed.
% The description says to place 14x31 = 434 coins on the chessboard each row containing 14
% coins and each column also containing 14 coins.
% """
%
% Cf the LPL model:
% http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
% 

% Since it is a linear programming model, eplex is very fast for the 
% 31/14 problem, as is minizinc (the ROTD version).
% fz, ic, fd and standard minizinc (fd) are all however slow.

 
int: n; % = 31; % the grid size
int: c; %  = 14; % number of coins per row/column
array[1..n, 1..n] of var 0..1: x; % the coin grid

% the sum of quadratic horizontal distance
var 0..n*c*n*c: z;

% solve satisfy;
% solve minimize z;
% indomain_median gives a much smaller start values (15420) for ic/fd
% solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_median, credit(961, lds(3))) minimize z;
solve :: int_search(
        [x[i,j] | i,j in 1..n] ++ [z], 
        most_constrained, % first_fail, 
        indomain_max, 
        complete) 
    minimize z;


constraint
  % rows
  forall(i in 1..n) (
    sum(j in 1..n) (x[i,j]) = c
  )
  /\ % columns
  forall(j in 1..n) ( 
    sum(i in 1..n) (x[i,j]) = c
  )
  /\ % quadratic horizonal distance
  z = sum(i,j in 1..n) (
     x[i,j]*(abs(i-j))*(abs(i-j)) 
  )
;

%
% data
%
n = 31; % 10;
c = 14; % 4


output 
[ 
  "z: ", show(z)
] ++
[
   if j = 1 then "\n" else " " endif ++
     show(x[i,j])
   | i,j in 1..n
] ++
[ "\n"];

