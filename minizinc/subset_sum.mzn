%
% Subset sum in Minizinc
%

% From Katta G. Murty: "Optimization Models for Decision Making", page 340
% http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
% 
% """
% Example 7.8.1
% 
% A bank van had several bags of coins, each containing either
% 16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
% street, thieves stole some bags. A total of 100 coins were lost.
% It is required to find how many bags were stolen.
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: n;
array[1..n] of int: coins;
array[1..n] of var int: x;
var int: ss = sum(i in 1..n) (x[i]);

int: total;

%
% subset_sub(values, x, tot) 
%  where 
%    values is the values to choose from (the coin values)
%    x contatins the resulting var 
%    total is the total value to sum
%
predicate subset_sum(array[int] of int: values, array[int] of var int: x, var int: tot) =
    sum(i in 1..n) (values[i]*x[i]) = tot /\ 
    forall(i in 1..n) (x[i] >= 0) 
;

solve satisfy;

constraint
    subset_sum(coins, x, total)
;

%
% data;
%
n = 6;
coins = [16, 17, 23, 24, 39, 40];
total = 100;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "ss: " ++ show(ss) ++ "\n" 
];