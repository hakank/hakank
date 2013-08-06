% 
% Curious set of integers problem in MiniZinc.
% 
% Martin Gardner (February 1967):
% """
% The integers 1,3,8, and 120 form a set with a remarkable priperty: the product of any two integers is one less than a perfect square. Find a fifth number that can be added to the set without destroying this property.
% """
% 
% Solution: The number is 0.
%
% There are however other sets of five numbers with this property.
% Here are the one in the range of 0.10000:
% [0, 1, 3, 8, 120]
% [0, 1, 3, 120, 1680]
% [0, 1, 8, 15, 528]
% [0, 1, 8, 120, 4095]
% [0, 1, 15, 24, 1520]
% [0, 1, 24, 35, 3480]
% [0, 1, 35, 48, 6888]
% [0, 2, 4, 12, 420]
% [0, 2, 12, 24, 2380]
% [0, 2, 24, 40, 7812]
% [0, 3, 5, 16, 1008]
% [0, 3, 8, 21, 2080]
% [0, 3, 16, 33, 6440]
% [0, 4, 6, 20, 1980]
% [0, 4, 12, 30, 5852]
% [0, 5, 7, 24, 3432]
% [0, 6, 8, 28, 5460]
% [0, 7, 9, 32, 8160]



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..1000: x;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

constraint
   all_different(x)
   /\
   increasing(x)
   /\
   forall(i, j in 1..n where i !=j) (
     let {
       var 0..10000: p
     } 
     in
      p*p-1 = (x[i]*x[j])
   )

   /\
   (
     (x[1] < 1 /\ x[2] = 1 /\ x[3] = 3 /\ x[4] = 8 /\ x[5] = 120)
     \/ 
     (x[1] = 1 /\ x[2] = 3 /\ x[3] = 8 /\ x[4] = 120 /\ x[5] > 120)
   )
;

output [
  show(x),"\n",

];
