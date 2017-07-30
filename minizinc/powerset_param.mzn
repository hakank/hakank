% 
% Powerset (param) in MiniZinc.
% 
% Generating powersets for 0..n-1

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% n -> powerset of 0..n-1 (translated to 1..n)
% Note: n=17 makes mzn2fzn segfault!
% n=16 is OK.
int: n = 5;

set of int: s = 0..n-1;
set of int: ss = 0..pow(2,n)-1;

% 0..n-1
% array[ss] of set of int: ps = array1d(ss,[{i | i in s where (k div pow(2,i)) mod 2 = 1} | k in ss]);
% Adding 1+ to get 1..n:
array[ss] of set of int: ps = array1d(ss,[{1+i | i in s where (k div pow(2,i)) mod 2 = 1} | k in ss]);

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


output [
  "s: " ++ show(s),"\n",
  % "ss: " ++ show(ss),"\n",
  % "ps: " ++ show(ps),"\n",
]
++
[
  show(ps[i])++": "++show(card(ps[i])) ++ "\n"
  | i in ss
]
;

