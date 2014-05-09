% 
% Sum of next natural numbers in MiniZinc.
% 
% Identify the sequence and sum of N natural increasing numbers.
%
% This is inspired by
%    http://www.hakank.org/minizinc/farmer_and_cows.mzn
% and 
%   https://oeis.org/A006003: n*(n^2 + 1) / 2
% """
% Write the natural numbers in groups: 1; 2,3; 4,5,6; 7,8,9,10; ... 
% and add the groups. In other words, "sum of the next n natural numbers".
% ...
% The sequence M(n) of magic constants for n X n magic squares 
% (numbered 1 through n^2) from n=3 begins M(n) = 15, 34, 65, 111, 175, 260, ..
% """


% The number of solutions (without the "identify constraint") for n is n*n-n.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 9; % the farmer and cows problem

% decision variables
array[1..n] of var 1..n*n: x;
var 0..n*n*n: z = sum(x);


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   alldifferent(x) /\
   increasing(x) /\
   x[n] - x[1] = n-1

   % "identify constraint"
   % identify the sequence for the "farmer and cows" solution
   /\ z = n*(n*n + 1) div 2
;

output [
  "z: " ++ show(z) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
];

