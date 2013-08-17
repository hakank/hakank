% 
% Multiplicative sequence in MiniZinc.
% 
% From "Mind Your Decision"
% "Monday puzzle: can you guess the 1000th number in my list?"
% http://mindyourdecisions.com/blog/2013/08/12/monday-puzzle-can-you-guess-the-1000-number-in-my-list/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+mindyourdecisions+%28Mind+Your+Decisions%29
% """
% I have written a long, long sequence of positive whole numbers. 
% Can you guess the 1,000 item on my list?
% 
% I will give you 3 clues about my sequence of numbers {a[n]}:
%   1. The list of numbers is strictly increasing (a[k] + 1 > a[k])
%   2. The second number in my list is 2 (a[2] = 2)
%   3. My list always has a(m*n) = a[m]*a[n] when the two numbers m and n are 
%      relatively prime. (Call this the multiplicative property). For 
%      example, a[60] = a[4]*a[15] because 4 and 15 are relatively prime.
% 
% What possible number(s) could I have written for a1000?
% 
% I admit I did not solve this puzzle on my own. Can you?
% 
% ...
% 
% Source of puzzle: this is puzzle 42 from the website qbyte.org [
%     http://www.qbyte.org/puzzles/puzzle05.html
%     "Multiplicative sequence"
% ]
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 1000;

array[1..n] of var 1..2*n: x;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i, j in 1..n where i < j /\ i*j <= n /\ i mod j > 0) (
     x[i*j] = x[i]*x[j]
  )
  /\
  forall(i in 2..n) ( x[i-1] < x[i] )
  % /\ increasing(x) % Testing
  /\
  x[2] = 2
;

output [
  "x: " ++ show(x) ++ "\n"
]
% ++
% [

%   show([i,j, x[i],x[j], x[i*j]]) ++ "\n"
%   | i, j in 1..m where i < j /\ i mod j > 0
% ]
;

