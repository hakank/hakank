% 
% n queens problem in MiniZinc.
% 
% This model has annotations for CP-Viz. Run as 
%     minizinc-viz -S queens.mzn
% 
% Note: This is supported in MiniZinc version 1.2.
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n;
array[1..n] of var 1..n: queens :: viz([
                                              % viztype("vector_waterfall"),
                                              viztype("vector"),
                                              vizpos(0,2),
                                              % vizdisplay("compact"),
                                              vizdisplay("expanded"),
                                              vizwidth(n),
                                              vizheight(n),
                                              vizrange(1,n)
                                              ]);

solve :: int_search(
        queens, 
        first_fail, 
        indomain_median, 
        complete) 
    satisfy;

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





