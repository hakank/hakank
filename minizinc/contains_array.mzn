% 
% Predicate contains in MiniZinc.
% 
% contains is a predicate for arrays which corresponds to "in" for sets.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 9;
array[1..n] of var 0..n: x;
var 0..n: el;
var int: num;
%
% array <-> number
%
predicate toNum(array[int] of var int: number, var int: num_sum, float: base) =
          let { 
             int: len = length(number) 
          }
          in
          num_sum = sum(i in 1..len) (
            ceil(pow(base, int2float(len-i))) * number[i]
          )
          /\ forall(i in 1..len) (number[i] >= 0)
;


%
% does a contains e?
%
predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   %el = 7
   %/\
   %cp1d([0,1,1,3,4,5,6,7,7], x)
   %/\
   % not(contains(el, x))
   contains(el, x)
   /\
   toNum(x, num, 10.0)
   /\
   num = 123402345
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "el: " ++ show(el) ++ "\n" ++
  "num: " ++ show(num)
];