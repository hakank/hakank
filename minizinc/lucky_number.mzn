% 
% Lucky numbers in MiniZinc.
% 
% From God Plays Dice
% "What is the origin of Kirillov's lucky number problem?"
% http://godplaysdice.blogspot.com/2011/03/what-is-origin-of-kirillovs-lucky.html
% """
% I've run across a few references to a Russian superstition as follows: 
% a bus ticket has a six-digit number, and a ticket is said to be 
% "lucky" if the sum of the first three digits equals the sum 
% of the last three digits. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 6;
int: m = n div 2;
array[1..n] of var 0..9: x;
% var 100000..999999: a;
% If "000000" etc counts as a ticket number:
var 0..999999: a;

predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   sum([x[i] | i in 1..m]) = sum([x[i] | i in m+1..n]) /\
   toNum10(x, a)
;


output [
  "x: " ++ show(x) ++ "\n" ++
  "a: " ++ show(a) ++ "\n"
] ++ ["\n"];