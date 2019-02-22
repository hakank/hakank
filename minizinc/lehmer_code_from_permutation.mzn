% 
% Lehmer code from permutation in MiniZinc.
% 
% From https://resources.wolframcloud.com/FunctionRepository/resources/LehmerCodeFromPermutation
% """
% The Lehmer code corresponding to Lehmer code perm is created.
% ...
% The generating algorithm is as follows. 
% For each number in the permutation, count how many subsequent values 
% it exceeds.
% 
% ...
% LehmerCodeFromPermutation[{1, 2, 9, 5, 4, 12, 7, 6, 3, 11, 10, 8}]
% {0, 0, 6, 2, 1, 6, 2, 1, 0, 2, 1, 0}
% """



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 12;

array[int] of int: a = [1, 2, 9, 5, 4, 12, 7, 6, 3, 11, 10, 8];


% decision variables
array[1..n] of var 0..n: x; % lehmer code

array[1..n] of var 1..n: y; % restore the original array

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i in 1..n) (
    x[i] = sum([a[j] < a[i] | j in i..n])
  )
  /\ % restore a
  forall(i in 1..n) (
    x[i] = sum([y[j] < y[i] | j in i..n])
  )
  /\ 
  all_different(y)
;

output [
 "a: \(a)\n",
 "x: \(x)\n",
 "y: \(y)",
];

