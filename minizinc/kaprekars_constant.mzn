% 
% Kaprekar's Constant in MiniZinc.
% 
% From
% http://www.math.hmc.edu/funfacts/ffiles/10002.5-8.shtml
% """
% Take any four digit number (whose digits are not all identical), and do the 
% following:
%
% 1. Rearrange the string of digits to form the largest and smallest 
%    4-digit numbers possible.
% 2. Take these two numbers and subtract the smaller number from the larger.
% 3. Use the number you obtain and repeat the above process. 
% 
% What happens if you repeat the above process over and over? Let's see...
% 
% Suppose we choose the number 3141.
% 4311-1134=3177.
% 7731-1377=6354.
% 6543-3456=3087.
% 8730-0378=8352.
% 8532-2358=6174.
% 7641-1467=6174...
% 
% The process eventually hits 6174 and then stays there! 
% 
% But the more amazing thing is this: every  four digit number whose digits are 
% not all the same will eventually hit 6174, in at most 7 steps, and then stay there! 
% """
% 
% Also see: 
%  - http://plus.maths.org/issue38/features/nishiyama/index.html
%  - http://en.wikipedia.org/wiki/6174_%28number%29
%
%
% Note: Here are the number of solutions with p steps
% for base = 10 and n = 4
%  iter  #solutions
%  1 (0)       1    (that is for start=6174)
%  2 (1)     383
%  3 (2)     576
%  4 (3)    2400
%  5 (4)    1272
%  6 (5)    1518
%  7 (6)    1656
%  8 (7)    2184
%  ------------
%           9990 (excluding the "all same" 4-digit numbers)
%
% Note: Here we skip the fixpoint number(s) so there are 
%       9989 solutions for n=4.

% 
% Number of solutions for different n:
%   n   #solutions (without fixpoint numbers)
%   3     989
%   4    9989
%   6   64468
%   8
% For n=6 this model yields 64468 solutions (took 25s with Gecode/fz)
%

% Also see this model where the end numbers are calculated:
%   http://www.hakank.org/minizinc/kaprekars_constant2.mzn
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
int: base = 10;
int: rows = base; % 7 steps for n = 4
int: max_val = ceil(pow(int2float(base), int2float(n)))-1; % 9999;
int: min_val = max_val div base;

% The end number(s) for base = 10 and n = 2..9.
% n >= 10 don't work well on a 32-bit machine.
% See http://www.hakank.org/minizinc/kaprekars_constant2.mzn
% for calculating the end number(s) for a base and length n.
%
array[2..10] of set of int: nums = 
 array1d(2..10,
 [ 
 {},                     % 2 none
 {495},                  % 3
 {6174},                 % 4
 {},                     % 5 none
 {549945, 631764},       % 6
 {},                     % 7 none
 {63317664, 97508421},   % 8
 {554999445, 864197532}, % 9
 {6333176664, 9753086421, 9975084201} % 10 % don't work on a 32-bit machine
 % {86431976532},         % 11   
 % {999750842001,997530864201,975330866421,633331766664,555499994445}, % 12
 % {8643319766532}        % 13
 ]);

% decision variables
array[1..rows] of var 0..max_val: x;

% where is the fixpoint (6174 for n=4)? This is the number of iterations. 
% There are pos-1 steps.
var 1..rows: pos; 

predicate reverse(array[int] of var 0..base-1: a, array[int] of var 0..base-1: rev) =
   let { 
      int: len = length(a)
   }
   in
   forall(i in 1..len) (
    rev[i] = a[len-i+1]
   )
;

predicate toNum(array[int] of var 0..base-1: a, var 0..max_val: n, int: base) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(int2float(base), int2float(len-i))) * a[i]
          )
;



% 
% do the Kaprakar procedure
%
predicate kaprekar(var 0..max_val: s, var 0..max_val: t, int: base) =
  let {
     array[1..n] of var 0..base-1: s_num,
     array[1..n] of var 0..base-1: s_ordered,
     array[1..n] of var 0..base-1: s_reverse, 
     var 0..max_val: ord_num,
     var 0..max_val: rev_num
  }
  in
  toNum(s_num, s, base)
  /\
  sort(s_num, s_ordered)
  /\
  reverse(s_ordered, s_reverse)
  /\
  toNum(s_ordered, ord_num, base)
  /\
  toNum(s_reverse,rev_num, base)
  /\
  t = rev_num - ord_num
;


% solve satisfy;
solve :: int_search(x ++ [pos], input_order, indomain_min, complete) satisfy;

% constraint  trace("min_val: " ++ show(min_val) ++ " max_val: " ++ show(max_val) ++ "\n" , 1=1);

constraint

  forall(i in 2..rows) (
     kaprekar(x[i-1], x[i], base)
  )

  % test one number
  /\ x[1] = 3141

  /\
  not(x[1] in nums[n]) % skip the fixpoint numbers
  /\ 
  % get the position
  let {
     var 1..rows: i
   } in
     pos = i
     /\
     x[i] in nums[n]
     /\
     (i > 1 -> x[i-1] != x[i])
   
 
  %/\ % get all cycles of length pos (i.e. pos-1 steps)
  %pos = 2
;

output [

  "x: " ++ show(x) ++ "\n" ++
  "pos: " ++ show(pos) ++ "\n" ++ 
  "start: " ++ show(x[1]) ++ "\n"
]
++
[
  show(x[i]) ++ " "
  | i in 1..fix(pos)
]
++ 
["\n"];