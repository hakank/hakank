% 
% Cube sum problem in MiniZinc.
% 
% From Krzysztof R. Apt & Mark G. Wallace 
% "Constraint Logic Programming using ECLiPSe", page 253
% """
% Problem: Given m check whether it can be written as a sum of at least two
% different cubes. If yes, produce the smallest solution in the number of
% cubes.
% """
%

% 
% Note: The ECLiPSe solution for m = 1000000 give the following result:
%  [16, 68, 88]
% i.e. an array with just 3 elements.
%
% MiniZinc has static arrays so the solution for m = 1000000 is
% [0, 0, 0, 0, 0, 0, 0, 16, 68, 88]
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: m = 1000000;
% int: m = 1000; % no solution
int: n = 10; % the maximum number of cubes
array[1..n] of var 0..100: x; % the cubes
var 0..n: z = sum(i in 1..n) (bool2int(x[i] > 0)); % number of cubes > 0

%
% all elements must be either different or 0.
%
predicate all_different_except_0(array[int] of var int: x) =
   let {
      int: n = length(x)
   }
   in
   forall(i,j in 1..n where i != j) (
        (x[i] > 0 /\ x[j] > 0) -> x[i] != x[j]
      
   )
;


% solve satisfy;
% solve minimize z;
solve :: int_search(x ++ [z], first_fail, indomain_min, complete) minimize z;

constraint
  all_different_except_0(x)
  /\
  increasing(x) % symmetry breaking
  /\ % sanity
  forall(i in 1..n) (
    x[i] >= 0 /\ x[i] <= m
  )
  /\
  m = sum(i in 1..n) (
    x[i]*x[i]*x[i] 
  )
  /\
  z >= 2 % at least two numbers
;


output [
  "x: ", show(x), "\n",
  "z: ", show(z), "\n",

];