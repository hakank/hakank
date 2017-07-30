% 
% Wolf, goat, and cabbage problem, linear programming version in MiniZinc.
% 
% A.k.a. river crossing problem. 
%
% Problem formulation from MiniZinc's example wolf_goat_cabbage.mzn 
% which uses a constraint programming approach.
% """"
% A farmer has to take a wolf, goat and cabbage across a bridge
% He can only take one thing at a time
% The wolf and goat can't be left together alone (without the farmer)
% The goat and cabbage can't be left alone together
% """
%
% This version use a linear programming version. Compare with e.g.
% http://www.hakank.org/minizinc/3_jugs2.mzn
% 
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

% For minimizing total_cost
% include "shortest_path_model.mzn";

% For showing all the solutions with total_cost <= 7
include "shortest_path_model_all.mzn";

n = 10; % 10 states
start = 1;
end = 8;
M = 999;

int: all = 1;

%
% data
%
d =
 % 1  2  3  4  5  6  7  8  8  10 
 [|M, 1, M, M, M, M, M, M, M, M,   % 1
  |1, M, 1, M, M, M, M, M, M, M,   % 2
  |M, 1, M, 1, M, M, M, M, M, 1,   % 3
  |M, M, 1, M, 1, M, M, M, M, M,   % 4
  |M, M, M, 1, M, 1, M, M, M, M,   % 5
  |M, M, M, M, 1, M, 1, M, 1, M,   % 6
  |M, M, M, M, M, 1, M, 1, M, M,   % 7
  |M, M, M, M, M, M, 1, M, 1, M,   % 8
  |M, M, M, M, M, 1, M, M, M, 1,   % 9
  |M, M, 1, M, M, M, M, M, 1, M,   % 10
|]
;

constraint total_cost <= 7 ;

% This works when using the minizinc solver, but not with other solvers.
output [
       if i = 1 /\ j = 1 then 
         "total_cost: " ++ show(total_cost) ++ "\n" ++
         "inFlow:  " ++ show(inFlow) ++ "\n" ++ "outFlow: " ++ show(outFlow) ++ "\n" ++
         "      1  2  3  4  5  6  7  8  9  0\n" 
       else "" endif ++
       if j = 1 then show_int(2, i) ++ " : " else "" endif ++
       show_int(2, x[i,j]) ++ if j = n then "\n" else " " endif
       | i in 1..n, j in 1..n
]
;
