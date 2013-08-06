%
% Assignment problem in Minizinc.
%
% Marriott & Stuckey "Programming with Constraints", page 266.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_workers = 4;
int: num_products = 4;
set of 1..num_workers: w = 1..num_workers;
set of 1..num_products: p = 1..num_products;

array[w, p] of int: profit;
array[w, p] of var 0..1: x;

var int: sum_profit = sum(i in w, j in p) (x[i,j]*profit[i,j]);


% solve satisfy;
solve maximize sum_profit;

constraint 
   forall(i in w) (
      sum(j in p) (x[i,j]) = 1
   )
   /\
   forall(j in p) (
      sum(i in w) (x[i,j]) = 1
   )
%   /\ sum_profit >= 21 % for solve satisfy
;

output [
   if  i = 1 /\ j = 1 then 
    "sum_profit: " ++ show(sum_profit) ++ "\n"
   else ""
   endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in w, j in p
] ++ ["\n"];

%
% data
%

profit =  [|7,1,3,4,
           |8,2,5,1,
           |4,3,7,2,
           |3,1,6,3|];
