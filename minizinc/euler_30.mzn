% 
% Euler problem 30 in MiniZinc.
% 
% """
% Surprisingly there are only three numbers that can be written as the 
% sum of fourth powers of their digits:
% 
%     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
%     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
%     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)
%
% As 1 = 1^(4) is not a sum it is not included.
%
% The sum of these numbers is 1634 + 8208 + 9474 = 19316.
%
% Find the sum of all the numbers that can be written as the sum of 
% fifth powers of their digits.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Answer: 443839

include "globals.mzn"; 
int: n = 5+1; % number of digits
int: m = 6;   % number of numbers (found by trial and error)
array[1..m, 1..n] of var 0..9: digits;
array[1..m] of var int: x;
var int: s = sum(x);

% Base = 10
predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(
        x,
        first_fail, 
        indomain_split, 
        complete) 
    satisfy;

constraint
    forall(i in 1..m) (
       toNum10([digits[i,j] | j in 1..n], x[i])
       /\
       x[i] = sum(j in 1..n) ( 
         digits[i,j]*digits[i,j]*digits[i,j]*digits[i,j]*digits[i,j] % n = 5
         % digits[i,j]*digits[i,j]*digits[i,j]*digits[i,j] % n = 4
       )
       /\
       x[i] > 1
    )
    /\ % symmetry breaking 
    increasing(x)
    /\
    all_different(x)
;


output [
   "s: ", show(s),"\n",
   "x: ", show(x), "\n",
]
++
[
  if j = 1 then "^5 \n" ++ show(x[i]) ++ " = " else "^5+ " endif ++
    show(digits[i,j]) 
  | i in 1..m, j in 1..n
];