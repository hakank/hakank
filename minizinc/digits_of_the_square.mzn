% 
% Digits of the Square puzzle in MiniZinc.
% 
% http://en.wikibooks.org/wiki/Puzzles/Arithmetical_puzzles/Digits_of_the_Square
% """
% There is one four-digit whole number n, such that the last four digits of 
% n^2 are in fact the original number n.
% 
% What is it?
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

var 1000..9999: n;
array[1..4] of var 0..9: n_a;
var 1000000..99980001: n_squared;
array[1..8] of var 0..9: n_squared_a;

predicate toNum(array[int] of var int: number, var int: num_sum,  float: base) =
          let { int: len = length(number) }
          in
          num_sum = sum(i in 1..len) (
            ceil(pow(base, int2float(len-i))) * number[i]
          )
          /\ forall(i in 1..len) (number[i] >= 0)
;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   % doing it the hard way ...
   toNum(n_a, n, 10.0)
   /\
   toNum(n_squared_a, n_squared, 10.0)
   /\
   forall(i in 5..8) (
      n_a[i-4] = n_squared_a[i]
   )
   /\
   n*n = n_squared
;

output
[
  "n: " ++ show(n) ++ "\n" ++ 
  "n_a: " ++ show(n_a) ++ "\n" ++ 
  "n_squared: " ++ show(n_squared) ++ "\n" ++ 
  "n_squared_a: " ++ show(n_squared_a) ++ "\n"
];