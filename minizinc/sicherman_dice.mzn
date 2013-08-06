% 
% Sicherman Dice in MiniZinc.
% 
% From http://en.wikipedia.org/wiki/Sicherman_dice
% """ 
% Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
% bear only positive integers, and have the same probability distribution for 
% the sum as normal dice.
% 
% The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
% """
%
% I read about this problem in a book/column by Martin Gardner long
% time ago, and got inspired to model it now by the WolframBlog post
% "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/
%
% This model gets the two different ways, first the standard way and
% then the Sicherman dice:
% 
% x1 = array1d(1..6, [1, 2, 3, 4, 5, 6]);
% x2 = array1d(1..6, [1, 2, 3, 4, 5, 6]);
% ----------
% x1 = array1d(1..6, [1, 2, 2, 3, 3, 4]);
% x2 = array1d(1..6, [1, 3, 4, 5, 6, 8]);
%
%
% Extra: If we also allow 0 (zero) as a valid value then the 
% following two solutions are also valid:
% 
% x1 = array1d(1..6, [0, 1, 1, 2, 2, 3]);
% x2 = array1d(1..6, [2, 4, 5, 6, 7, 9]);
% ----------
% x1 = array1d(1..6, [0, 1, 2, 3, 4, 5]);
% x2 = array1d(1..6, [2, 3, 4, 5, 6, 7]);
% 
% These two extra cases are mentioned here:
% http://mathworld.wolfram.com/SichermanDice.html
%
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
% 

include "globals.mzn"; 
int: n = 6;
int: m = 10; % max integer 

% standard distribution
array[2..12] of int: standard_dist = array1d(2..12, [1,2,3,4,5,6,5,4,3,2,1]);

% the two dice
array[1..n] of var 1..m: x1;
array[1..n] of var 1..m: x2;

% solve satisfy;
solve :: int_search(x1 ++ x2, smallest, indomain_min, complete) satisfy;

constraint
 
   forall(k in 2..12) (
       standard_dist[k] = sum(i,j in 1..n) ( bool2int(x1[i]+x2[j] == k))
   )

   % symmetry breaking
   /\ increasing(x1) 
   /\ increasing(x2)

   /\ % x1 is less or equal to x2
   forall(i in 1..n) (
      x1[i] <= x2[i]
   )
   
   % /\ lex_lesseq(x1, x2)
;


output 
[
  "x1" ++ show(x1) ++ "\n" ++
  "x2" ++ show(x2) ++ "\n" 
];
