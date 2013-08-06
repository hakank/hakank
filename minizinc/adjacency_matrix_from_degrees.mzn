% 
% Adjacency matrix (graph) from degrees in MiniZinc.
% 
% From Stack Overflow
% Anyone has the logic for the following puzzle
% http://stackoverflow.com/questions/6789797/anyone-has-the-logic-for-the-following-puzzle
% """
% I have an n*n matrix. Each vertex has a degree associated with it. Degree is the 
% number of lines that can be drawn to its neighbouring vertices. I am generating an 
% array containing degrees of each vertex. For example, array {1,2,2,1} implements the 
% following two solutions.
%
% solution 1
%    [ In matrix form
%        1 2 3 4
%        -------
%      1|0 1 0 0   1
%      2|1 0 1 0   2
%      3|0 1 0 1   2
%      4|0 0 1 0   1
%     
%        1 2 2 1
%    ]
% solution 2
%    [ In matrix form
%        1 2 3 4
%        -------
%      1|0 0 1 0   1
%      2|0 0 1 1   2
%      3|1 1 0 0   2
%      4|0 1 0 0   1
% 
%        1 2 2 1
% 
%    ]
% 
% what i want is, when i get the array i want to know whether it has one solution or more 
% than one solution.
%
% This is another example {0, 3, 1, 2, 4, 2, 2, 1, 3} has more 
% than one solutions.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% 2 solutions
int: n = 4;
array[1..n] of int: degrees = [1,2,2,1];

% the other example: 737 solutions
% int: n = 9;
% array[1..n] of int: degrees = [0, 3, 1, 2, 4, 2, 2, 1, 3];


% decision variables (adjacency matrix)
array[1..n, 1..n] of var 0..1: x;


solve satisfy;
% solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint
  % row sums == degrees and col sums == degrees
  forall(i in 1..n) (
     sum(j in 1..n) (x[i,j]) = degrees[i]
     /\
     sum(j in 1..n) (x[j,i]) = degrees[i]
    
     /\
     x[i,i] = 0 % no self loops
  )
;

% the matrix is symmetric
constraint
  forall(i,j in 1..n) (
     x[i,j] = 1 <-> x[j,i] = 1
  )
;

output [
   if j = 1 then "\n" else " " endif ++
     show(x[i,j])
   | i,j in 1..n
]
 ++ ["\n"]
;

