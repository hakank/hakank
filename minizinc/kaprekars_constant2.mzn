% 
% Kaprekars constant (calculation of) in MiniZinc.
% 
% This MiniZinc model calculates the Kaprekars constant
% for a specific base and length of the numbers.
%
% See http://plus.maths.org/issue38/features/nishiyama/index.html
% for a discussion.
%
% For base = 10 (this list is from the above cited article)
% Digits  Kernel
% 2       None
% 3       495
% 4       6174
% 5       None
% 6       549945, 631764
% 7       None
% 8       63317664, 97508421
% 9       554999445, 864197532
% 10      6333176664, 9753086421, 9975084201
% 11      86431976532
% 12      999750842001,997530864201,975330866421,633331766664,555499994445
% 13      8643319766532

% Also see: http://en.wikipedia.org/wiki/6174_%28number%29

% Note: for base >= 10 and n = 10 this model gives overflow
% "The bounds on this expression exceed what can be represented on a 32 bit
% machine."
% G12/fd can happily solve for n=10..12

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 4; % length of the number y, i.e. for n = 4: 1000..9999
int: base = 10; % base of the number
int: max_val = ceil(pow(int2float(base), int2float(n)))-1; % 9999
int: min_val = max_val div base;

% decision variables
array[1..n] of var 0..base-1: y; %;
var min_val..max_val: y_num; %;
array[1..n] of var 0..base-1: y_rev; %;
var int: y_rev_num; %;
array[1..n] of var 0..base-1: x;%; % result
var int: x_num;

array[0..base-1] of var 0..n: y_gcc; %;
array[0..base-1] of var 0..n: x_gcc; %;


predicate toNum(array[int] of var 0..base-1: a, var 0..max_val: n, int: base) =
    let { 
       int: len = length(a) 
    } in
    n = sum(i in 1..len) (
      ceil(pow(int2float(base), int2float(len-i))) * a[i]
    )
;

predicate reverse(array[int] of var 0..base-1: a, array[int] of var 0..base-1: rev) =
   let { 
      int: len = length(a)
   }
   in
   forall(i in 1..len) (
     rev[i] = a[len-i+1]
   )
;

predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;


predicate not_all_same(array[int] of var int: x) =
   not(forall(i in 2..length(x)) (
           x[i] = x[i-1]
         )
      )
;

% solve satisfy;
solve :: int_search(x ++ y, first_fail, indomain_max, complete) satisfy;

constraint
   not_all_same(y) /\
   toNum(y, y_num, base) /\
   reverse(y, y_rev) /\
   toNum(y_rev, y_rev_num,base) /\
   
   toNum(x, x_num, base) /\
   y_num > y_rev_num /\
   x_num = y_num - y_rev_num /\

   % y is decreasing
   decreasing(y) /\
   base-1 >= y[1]

   /\ % exactly the same digits (in some order) in
      % y and x
   global_cardinality(y, array1d(0..base-1, set2array(index_set(y_gcc))), y_gcc) /\
   global_cardinality(x, array1d(0..base-1, set2array(index_set(x_gcc))), x_gcc) /\
   forall(i in 0..base-1) (
      y_gcc[i] = x_gcc[i]
   ) 
;


output [
  "n: " ++ show(n) ++ "\n" ++
  "base: " ++ show(base) ++ "\n" ++
  "x_num: " ++ show(x_num) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "x: " ++ show(y) ++ "\n"
] 
++
["\n"];

