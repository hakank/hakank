% 
% Magic Squares and Cards in MiniZinc.
% 
% Martin Gardner (July 1971)
% """
% Allowing duplicates values, what is the largest constant sum for an order-3
% magic square that can be formed with nine cards from the deck.
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 3;
array[1..n, 1..n] of var 1..13: x;
var 0..13*4: s;

solve satisfy;
% solve maximize s;
% solve :: int_search([x[r,c] | r,c in 1..n], first_fail, indomain_min, complete) maximize s;

constraint

  % there are 4 cards of each value in a deck
  forall(i in 1..13) (
     % at_most(4, [x[r,c] | r,c in 1..n], i)
     count(t in [x[r,c] | r,c in 1..n]) (t = i) <= 4
  )

  % the standard magic square constraints (sans all_different)
  /\
  forall (c in 1..n) (sum (r in 1..n) (x[r, c]) = s)
  /\
  forall (r in 1..n) (sum (c in 1..n) (x[r, c]) = s)
  /\
  sum (i in 1..n) (x[i, i]) = s
  /\
  sum (i in 1..n) (x[i, n + 1 - i]) = s
;


output 
[
  "\ns: ", show(s)
] ++
[
  if c = 1 then "\n" else " " endif ++
    show(x[r,c])
  | r, c in 1..n
]
++ ["\n"]
;
