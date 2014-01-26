% 
% Chinese Remainder Problem in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Chinese_remainder_theorem
% """
% In its basic form, the Chinese remainder theorem will determine a 
% number n that when divided by some given divisors leaves given 
% remainders. For example, what is the lowest number n that when 
% divided by 3 leaves a remainder of 2, when divided by 5 leaves a 
% remainder of 3, and when divided by 7 leaves a remainder of 2?
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n;
array[1..n] of int: divs;
array[1..n] of int: rems;

% decision variables
var 1..10000: x;

solve minimize x;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

predicate crt(array[int] of var int: divs, 
              array[int] of var int: rems,
              var int: n) =
  forall(i in index_set(divs)) (
     n mod divs[i] = rems[i]
  )

;

constraint
  crt(divs,rems, x)
;

output [
  "divs: ", show(divs), "\n", 
  "rems: ", show(rems), "\n",
  show(x)
];


% n = 3;
% divs = [3,5,7];
% rems = [2,3,2];

n = 5;
divs = [3,5,7,11,13];
rems = [2,3,2,4,1];