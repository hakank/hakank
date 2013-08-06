% 
% n queens problem in MiniZinc.
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n;
array[1..n] of var 1..n: queens;

solve :: int_search(
        queens, 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

% solve satisfy;

constraint all_different(queens);

constraint
    forall(i, j in 1..n where i < j) (
         queens[i] != queens[j] /\
         queens[i] + i != queens[j] + j /\
         queens[i] - i != queens[j] - j
    ) 
 ;

output [
  show(queens) ++ "\n"

];

% data
n = 8;





