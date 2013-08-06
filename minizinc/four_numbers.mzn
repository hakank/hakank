% 
% Four number problem in MiniZinc.
% 
% From
% http://stackoverflow.com/questions/17720465/given-4-numbers-of-array-of-1-to-10-elements-find-3-numbers-whose-sum-can-gener
% "Given 4 numbers of array of 1 to 10 elements. 
%  Find 3 numbers whose sum can generate all the four numbers?"
% """
% I am given an array containing a list of 4 random numbers (1 to 10 inclusive). 
% I am supposed to generate a list of 3 numbers (1 to 10 inclusive) so that I can 
% generate all the 4 numbers of the initial list by adding the 3 numbers of 
% the generated list.
% Someone Please provide an algorithm for doing this. 
% """
% 
% For the problem instance mentioned in a comment [1,3,7,8], there are 5 solutions:
%
%   r: [1, 3, 7, 8]
%   x: [1, 3, 4]
%   ----------
%   r: [1, 3, 7, 8]
%   x: [1, 2, 5]
%   ----------
%   r: [1, 3, 7, 8]
%   x: [1, 2, 6]
%   ----------
%   r: [1, 3, 7, 8]
%   x: [1, 2, 7]
%   ----------
%   r: [1, 3, 7, 8]
%   x: [1, 3, 7]



% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: m = 4;
int: n = 3;

% array[1..m] of int: r = [1,3,7,8]; % "random" numbers
% array[1..m] of int: r = [1,3,7,9];
% array[1..m] of int: r = [1,3,7,9]; %
% array[1..m] of int: r = [1,6,7,9];
% array[1..m] of int: r = [4,6,7,9];
array[1..m] of int: r = [5,6,7,9];
% array[1..m] of int: r = [1,2,4,8]; % No solution found

% array[1..m] of var 1..10: r; % "random" numbers

% decision variables
array[1..n] of var 1..10: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  forall(i in 1..m) (
     let {
       array[1..n] of var 0..1: tmp
     } in
     sum(j in 1..n) (tmp[j]*x[j]) = r[i]
  )
  % symmetry breaking
  % /\ alldifferent(r) % assumption on the random numbers
  % /\ increasing(r)

  /\ increasing(x)
;

output [
  "r: " ++ show(r) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
];

