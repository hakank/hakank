% 
% Average avoiding in MiniZinc.
% 
% From Stack Overflow
% http://stackoverflow.com/questions/19345090/arrange-an-array-such-that-the-average-of-2-numbers-does-not-lie-between-them
% """
% As the question says, find an algorithm to arrange the array. This was a 
% Facebook interview question.
% 
% The average needs to be exact. We do not round of, take floor or ceil of the average.
% 
% Edit : To quote an example, if the numbers are 1,2,5,9, then the arrangement 
% {1,9,2,5} is valid but {1,5,9,2} is not since average of 1 and 9 is 5 and lies 
% between them. Why is everybody downvoting this?
% 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 4;
% array[1..n] of int: a = [1,2,5,9];
% int: n = 9;
% array[1..n] of int: a = [1,2,3,4,5,6,7,8,9];
int: n = 3;
array[1..n] of int: a = [2,3,4];

set of int: s = { a[i] | i in 1..n  };
% array[1..card(s)] of int: counts = [ sum([bool2int(a[i] == v) | i in 1..n ]) | v in s];

% decision variables
array[1..n] of var s: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i,j in 1..n where i != j) (
    forall(k in i+1..j-1) (
       2*x[k] != (x[i] + x[j])
    )    
  )
  /\ % count occurrences of v in a
     % (since we can't assume distinct values)
  forall(v in s) (
     % let {
     %   int: c = sum([bool2int(a[j] == v) | j in 1..n])
     % } in
     let {
       var 1..n: c
     } in
     count(a, v, c) /\
     count(x, v, c)
  ) 

  % symmetry breaking (TESTING)
  % /\ x[1] = a[1]

;

output [
  "a: " ++ show(a) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
];

