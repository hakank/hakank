%
% Shortest path (transshipment) problem in Minizinc.
%
% From Winston "Operations Research", page 414 and 417.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "shortest_path_model.mzn";

n = 6;
start = 1;
end= 6;
M = 9999;

d =
array2d(1..n, 1..n, [
 0, 4, 3, M, M, M,
 M, 0, M, 3, 2, M,
 M, M, 0, M, 3, M,
 M, M, M, 0, M, 2,
 M, M, M, M, 0, 2,
 M, M, M, M, M, 0
]);

output [
       if i = 1 /\ j = 1 then
         "total_cost: " ++ show(total_cost) ++ "\n" ++
         "inFlow:  " ++ show(inFlow) ++ "\n" ++ "outFlow: " ++ show(outFlow) ++ "\n" ++
         "    1 2 3 4 5 6\n" 
       else "" endif ++
       if j = 1 then show(i) ++ " : " else "" endif ++
       show_int(4,x[i,j]) ++ if j = n then "\n" else " " endif
       | i in 1..n, j in 1..n
];
