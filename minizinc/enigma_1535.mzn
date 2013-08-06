% 
% Enigma problem 1535 (Back to front) in MiniZinc.
%
% From New Scientist:
% """
% Back to front
%
% Enigma 1535 Bob Walker, New Scientist magazine, March 7, 2009.
%
% Joe has found that just a few numbers have a rather unique property. 
% Reverse the order of the digits, and the new number is a multiple of the
% original. For some specified numbers of digits (four or more) there are only
% two numbers with the property.
% 
% Joe gave Penny the task of finding the two 6-digit numbers.
% What is the lower 6-digit number?
% """
% 
% Compare to the Formula One model:
% http://www.f1compiler.com/samples/Enigma%201535.f1.html

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:% www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 6;
var 100000..999999: x1;
var 100000..999999: x2;

array[1..n] of var 0..9: x1_a;
array[1..n] of var 0..9: x2_a;

% reverse an array
predicate reverse(array[int] of var int: x, array[int] of var int: rev) =
        let { int: len = card(index_set(x)) } in
        forall(i in index_set(x)) (
           rev[i] = x[len-i+1]
        )
;

% convert array <-> number
predicate toNum(array[int] of var int: a, var int: n) =
          let { int: len = length(a) } in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(x1_a ++ x2_a ++ [x1,x2], first_fail, indomain_min, complete) satisfy;

constraint
  toNum(x1_a, x1) /\
  toNum(x2_a, x2) /\
  reverse(x1_a, x2_a) 
  /\
  x1 > x2 /\
  x1 mod x2 = 0
;


output
[
  "x1: " ++ show(x1) ++ "\n" ++
  "x2: " ++ show(x2) ++ "\n" 
];