% 
% Twelve statemments problem in MiniZinc.
% 
% http://blog.tanyakhovanova.com/2015/07/kvantiks-problems-solutions/
% """
% Problem 5. There are 12 cards with the statements 
%    "There is exactly one false statement to the left of me," 
%    "There are exactly two false statements to the left of me,"
%    ..., 
%    "There are 12 false statements to the left of me." 
% Pete put the cards in a row from left to right in some order. What is the largest number 
% of statements that might be true?
%
% Solution. Suppose there are more than six statements that are true. That means one of 
% the cards with a number more than six is true, meaning that there are at least 7 
% false statements. This is a contradiction. Suppose there are 6 true cards. They have 
% to be cards with numbers one through six. In addition, no pair of adjacent cards can both 
% be true. Arranging the cards in the order 7, 1, 8, 2, 9, 3, 10, 4, 11, 5, 12, 6 works. 
% Notice that we can permute the cards with numbers more than 6 and it will still work.
% """

%
% Here is one optimal solution:
%  x: [7, 1, 8, 2, 9, 3, 10, 4, 11, 5, 12, 6]
%  y: [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
%  z: 6
%
% Without symmetry breaking there are 720 (6!) optimal solutions to this puzzle.
% With symmetry breaking there is a unique solution: the one above.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 12;


% decision variables
array[1..n] of var 1..n: x;
array[1..n] of var 0..1: y;
var 0..n: z = sum(y);

% 
% Ensures that array a is increasing if we disregard any 0's
% 
predicate increasing_except_0(array[int] of var int: a) =
    forall(i,j in index_set(a) where i < j) (
       (a[i] != 0 /\ a[j] != 0) -> (a[i] <= a[j])
    )
;


% solve maximize z;
% solve satisfy;
solve :: int_search(y ++ x, first_fail, indomain_max, complete) maximize z;

constraint
   all_different(x) /\
   forall(i in 1..n) (
      x[i] = sum([bool2int(y[j]=0) | j in 1..i-1]) <-> y[i] = 1
   )

   /\ % symmetry breaking: order the false statements
   increasing_except_0([x[i]*(y[i]=0) | i in 1..n])

   % /\ z = 6 % for solve satisfy

;

output [
  "x: ", show(x), "\n",
  "y: ", show(y), "\n",
  "z: ", show(z), "\n",
];

