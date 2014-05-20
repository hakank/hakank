% 
% 225 divisor problem in MiniZinc.
% 
% From http://www.quora.com/Mathematics/What-is-the-smallest-number-divisible-by-225-that-consists-of-all-1s-and-0s
% """
% What is the smallest number divisible by 225 that consists of all 1s and 0s?
% """

% Note: Not many systems supports this large domain. Here are some 
% that do:
%   - g12/fd
%   - ECLiPSe/ic
%   - or-gools/fz
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 11;

% decision variables
array[1..n] of var 0..1: x; % restrict to just 0 and 1
var 1..pow(10, n)-1: y;


predicate to_num(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            pow(10, len-i) * a[i]
          )
;


% solve minimize y;
solve :: int_search(x, first_fail, indomain_min, complete) minimize y;

constraint
   to_num(x, y)
   /\
   y mod 225 = 0
;

output [
  "x: ", show(x), "\n",
  "y: ", show(y)
];

