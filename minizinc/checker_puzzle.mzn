% 
% Checker puzzle in MiniZinc.
% 
% From http://mathoverflow.net/questions/1947/placing-checkers-with-some-restrictions
% Placing checkers with some restrictions
% """
% We are going to put n checkers on an (n x n) checkers board, 
%  with the following restrictions:
%
% 1) In each column there is EXACTLY one checker.
% 
% 2) For i=1,2,...,(n-1), the first i rows cannot have EXACTLY i checkers.
% 
% The question is to count the number of ways to do so. I guess that the 
% answer is n^{n-1}, but I do not know how to prove it. Can anyone help?
%
% (If restriction 2) is removed, the answer is obviously n^n.)
% """
% 

% Here is the result:
%  n   #solutions
%  --------------
%  1        1
%  2        2
%  3        9
%  4       64
%  5      625  
%  6     7776
%  7   117649
%  8  2097152
% 
% which confirms the hypothesis:
%
% > [n^(n-1) | n<-[1..10]]
% [1,2,9,64,625,7776,117649,2097152,43046721,1000000000]
%
%
% Number of the different row patterns
% ------------------------------------
% 
%  for n = 3  there are 5 different patterns
%   #  pattern
%   1 [0, 0, 3]
%   1 [0, 3, 0]
%   1 [3, 0, 0]
%   3 [0, 1, 2]
%   3 [2, 1, 0]
%
% for n = 4 there are 14 different patterns
%   1 [0, 0, 0, 4]
%   1 [0, 0, 4, 0]
%   1 [0, 4, 0, 0]
%   1 [4, 0, 0, 0]
%   4 [0, 0, 1, 3]
%   4 [0, 1, 0, 3]
%   4 [0, 1, 3, 0]
%   4 [0, 3, 1, 0]
%   4 [3, 0, 1, 0]
%   4 [3, 1, 0, 0]
%   6 [0, 0, 2, 2]
%   6 [2, 2, 0, 0]
%  12 [0, 1, 1, 2]
%  12 [2, 1, 1, 0]
%
% for n = 5 there are  42 different patterns
%     n = 6           132
%     n = 7           429
%
% And this seems to be the Catalan numbers...
% http://en.wikipedia.org/wiki/Catalan_number
% 
% 1,2,5,14,42,132,429,...

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;

% comment the output for counting larger values of n
array[1..n, 1..n] of var 0..1: x;
array[1..n] of var 0..n: sum_rows;

solve :: int_search([x[i,j] | i,j in 1..n] ++ sum_rows, first_fail, indomain_min, complete) satisfy;

constraint

  % We are going to put n checkers on an (n x n) checkers board, 
  sum([x[i,j] | i,j in 1..n]) = n

  /\ % In each column there is EXACTLY one checker.
  forall(j in 1..n) (
      sum([x[i,j] | i in 1..n]) = 1
  )

  /\ % For i=1,2,...,(n-1), the first i rows cannot have EXACTLY i checkers.
  forall(i in 1..n) (
    % number of checker on each row 
    sum_rows[i] = sum([x[i,j] | j in 1..n])
  )
  /\
  forall(i in 1..n-1) (
      sum([sum_rows[j] | j in 1..i]) != i
  ) 
;

output 
[
  "sum_rows: " ++ show(sum_rows) ++ "\n"
] 
++
[
  if j = 1 then  "\n" else " " endif ++
    show(x[i,j])
  | i, j in 1..n
]
++ 
["\n"];