% 
% Divisible by 1 through 9 problem in MiniZinc.
%
% http://mindyourdecisions.com/blog/2016/04/10/find-the-10-digit-number-where-n-digits-are-divisible-by-n-sunday-puzzle/
% """
% "
% Find a 10 digit number that uses each of the digits 0 to 9 exactly once and 
% where the number formed by the first n digits of the number is divisible by n.
% "
% You should read the digits of the number from left to right. For example, in the 
% number abcd, you need the number a to be divisible by 1, the number ab to be 
% divisible by 2, the number abc to be divisible by 3, and the entire number abcd 
% to be divisible by 4.
% """
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: base = 10;
int: n = base;
int: m = ceil(pow(int2float(base),(int2float(n))))-1;
array[1..n] of var 0..n-1 : x; % the digits
array[1..n] of var 0..m : t; % the numbers. t[1] contains the answer

predicate toNum(array[int] of var int: a, var int: n, int: base) =
  let { int: len = length(a) } in
  n = sum(i in 1..len) (
     pow(base, len-i) * a[i]
  )
;

% solve satisfy;
% solve :: int_search(t ++ x, occurrence, indomain_max, complete) satisfy;
solve :: int_search(x, occurrence, indomain_max, complete) satisfy;

constraint
   all_different(x) /\
   forall(i in 1..n) (
       toNum([x[j] | j in 1..i], t[i], base) /\
       t[i] mod i = 0
   )    
;


output [
  "x: " ++ show(x) ++ "\n" ++
  "t: " ++ show(t) ++ "\n"
] 
++ ["\n"];