% 
% Yet another coin problem in MiniZinc.
% 
% From 
% http://forum.openopt.org/viewtopic.php?id=5
% """
% In my pocket I have £41.58 which is made up of different denominations of coins. 
% There is exactly the same number of each coin. What is the minimum number of coins 
% I have, and what are they? (Britain has 8 commonly used coins, and in GBP their 
% values are: 0.01, 0.02, 0.05, 0.10, 0.20, 0.50, 1, 2).
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 8;
array[1..n] of int: denom = [1, 2, 5, 10, 20, 50, 100, 200]; % in cents
int: s = 4158; % 41.58
array[1..n] of var int: x;
var int: num_coins = sum(i in 1..n) (x[i]);

solve :: int_search(x, smallest, indomain_min, complete) minimize num_coins;
% solve :: int_search(x, smallest, indomain_min, complete) satisfy;

constraint
%   s = 4158
%   /\
   sum(i in 1..n) (x[i]*denom[i]) = s
   /\
   forall(i in 1..n) (
      x[i] >= 0
      /\
      x[i] <= s
   )
   /\
   forall(i, j in 1..n) (
     x[i] = x[j] 
     \/ 
     x[i] = 0 
     \/
     x[j] = 0
   )
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "num_coins: " ++ show(num_coins)
];