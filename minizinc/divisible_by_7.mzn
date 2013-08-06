% 
% Divisible by 7 in MiniZinc.
% 
% (Attributed to Sam Loyd)
% """
% Three numbers (6, 3 and 1) are drawn on the sides of three cubes - a number 
% per cube. Can you arrange the three cubes in a line so that to create a 
% 3-digit number divisible by 7? Each cube must be employed.
% 
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 3;
% The trick is - of course - that the 6 cube could be 9 as well
array[1..n] of var {1,3,6,9}: x;
var int: z = 100*x[1] + 10*x[2] + x[3];

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

predicate countx(array[int] of var int: x, var int: y, var int: c) =
    c = sum(i in index_set(x)) ( bool2int(x[i] == y) )
;

constraint

  all_different(x)
  /\
  z mod 7 = 0
  /\
  % cannot have both a 6 and 9
  (
    (countx(x, 6, 1) /\ countx(x, 9, 0))
    \/ 
    (countx(x, 9, 1) /\ countx(x, 6, 0))
  )
%  /\ % this is not as efficient as the count construct above
%  (
%                       %   1 2 3 4 5 6 7 8 9
%    global_cardinality(x, [1,0,1,0,0,1,0,0,0])
%    \/
%    global_cardinality(x, [1,0,1,0,0,0,0,0,1])
%  )

;

output
[
  "x: " ++ show(x)
];