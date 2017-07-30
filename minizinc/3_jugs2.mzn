% 
% Minizinc model for 3 jugs problem
%
% Model from 
% Taha "Introduction to Operations Research", page 245f .
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "shortest_path_model.mzn";

n     = 15;  % number of nodes ( n x n matrix)
start = 1;   % start node
end   = 15;  % end node
M     = 999; % large number

% distance
d =
array2d(1..n, 1..n,
[
  M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M,
  M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M,
  M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M,
  M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M,
  M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M,
  M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M,
  M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M,
  M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
  M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M,
  M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M,
  M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M,
  M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M,
  M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M,
  M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1, 
  M, M, M, M, M, M, M, M, M, M, M, M, M, M, M
])
;

%
% output connection matrix and more.
% (maybe this should be in the shortest_path_model.mzn instead since it's
%  quite general...)
%
output [
       if i = 1 /\ j = 1 then 
         "rhs: " ++ show(rhs) ++ "\n" ++
         "total_cost: " ++ show(total_cost) ++ "\n" ++
         "inFlow:  " ++ show(inFlow) ++ "\n" ++ "outFlow: " ++ show(outFlow) ++ "\n" ++
         "    1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n" 
       else "" endif ++
       if j = 1 then show(i) ++ " : " else "" endif ++
       show(x[i,j]) ++ if j = n then "\n" else " " endif
       | i in 1..n, j in 1..n
];
