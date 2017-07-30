% 
% Pairs divides the sum puzzle in MiniZinc.
% 
% From comp.lang.prolog
% """
% Date: Sat, Feb 28 2009 3:55Â am
% From: Nick Wedd
%
% Here is a puzzle which I found surprisingly easy to program Prolog to
% generate solutions to.  If any of you teach Prolog to students, you
% might use it as an example (like the goat-wolf-cabbage thing).
%
% Find a set of four distinct positive integers such that, for every pair
% of them, their difference divides their sum.
%
% Find lots of such sets.
%
% As above, but sets of five distinct positive integers.
% 
% As above, but sets of six ...
% """
%
% Compare with the following model:
% * Comet:  http://www.hakank.org/comet/pair_divides_the_sum.co


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 7;
int: max_val = 100;

% decision variables
array[1..n] of var 1..max_val: x;
var 1..max_val*n: z;



% solve satisfy;
solve :: int_search(x, max_regret, indomain_min, complete) satisfy;

constraint
  alldifferent(x) /\

  % ordered
  increasing(x) /\

  z = sum(x) /\

  z mod n = 0 /\

  forall(i,j in 1..n where i < j) (
    %  abs(x[i]-x[j]) > 1 /\ % extra: not allowing differences of 1.
    z mod abs(x[i]-x[j]) = 0
  )
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];