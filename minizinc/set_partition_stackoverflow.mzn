%
% Set partition problem in Minizinc.
% 
% I'm not sure if this is a set partition problem but let's play 
% a little. 
% 
% "fast and simple constraint programming involving vectors (arrays)" 
% http://stackoverflow.com/questions/16430091/fast-and-simple-constraint-programming-involving-vectors-arrays?utm_source=twitterfeed&utm_medium=twitter
% """
% I have many,many, many vectors that I need to check for duplicates of numbers with 
% some very basic first order logic.
% 
% I can use intersections, but that is proving to be too slow. I thought I could turn this 
% into a bitwise problem. The full set of integers are known and each vector/array could be 
% represented as a bitset, but I can only find a half a solution.
% 
% I currently use looping and vector intersection but it is proving to be too slow 
% for the amount of problems I need to check.
% 
% For a simple example, given:
% 
% E: 1 2
% F: 2 4
% M: 1 3
% N: 4 5  
% A: 5 6
% 
% The problem I am trying to identify, is always a larger format of:
% 
% (E || F) && (M || N) && A -> which is proven as possible by selecting F,M,A.
% 
% I need to verify if the above is possible without duplicates.
% 
% Is there a means of examining vectors/arrays like this that is faster than 
% 9 million loops? Are the constraint libraries the only option?
% 
% In an effort to clarify:
% 
% containers are std::vector.
% 
% The vectors contain any integer.
% 
% I would need to examine them problem by problem to identify the full set of integers.
% 
% Using the conditional logic specified to select entire vectors, will a duplicate 
% occur? The conditional operators in use would always be "AND" and "OR" only. 
% The problem I listed is a simplified version, but that is really all there is to it. 
% It just differs in size.
% 
% The output I care less about..it could be a boolean, another vector of potential 
% duplicates, etc. I am trying to find the right tool for the job rather than salvaging.
%
% In my current set up, I would solve this by analyzing for forced items like A 
% and removing anything it intersects with...(in this case, N...then I would loop 
% again, and do the same process with M, which is now a forced choice, and removing 
% E, leaving me with F.
% """

% Note: I'm testing just the simple problem shown above
% """
%   E: 1 2
%   F: 2 4
%   M: 1 3
%   N: 4 5  
%   A: 5 6
% 
% The problem I am trying to identify, is always a larger format of:
% 
% (E || F) && (M || N) && A -> which is proven as possible by selecting F,M,A.
% """

% This model give the following solution:
%
%   x: [false, true, true, false, true]
%   x2: [{}, {2, 4}, {1, 3}, {}, 5..6]
%
% Which seems to the one looking for.


%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 5; % number of sets

array[1..n] of set of int: s = 
  [
   {1,2}, % E
   {2,4}, % F
   {1,3}, % M
   {4,5}, % N 
   {5,6}  % A
  ];

% All values (the "universe")
set of int: values = {j | i in 1..n, j in s[i]};

% decision variables
array[1..n] of var bool: x; % which set (in s) to select
array[1..n] of var set of values: xs; % the selected sets

solve satisfy;
% Minimize the number of selected sets
% solve minimize sum(i in 1..n) (bool2int(card(xs[i]) > 0));

constraint 

  % The condition
  % (E || F) && (M || N) && A
  ((x[1] \/ x[2]) /\ (x[3] \/ x[4]) /\ x[5])
  /\ 
  forall(i in 1..n) (
     % If this set is selected (in x[i]), put s[i] in xs[i]
     (x[i] <-> xs[i] = s[i])
     /\ % ensure not selected sets are represented as {} in xs
     (not(x[i]) <-> card(xs[i]) = 0)
  )
  /\ % make sure that a value is selected in exactly one set
  partition_set([xs[i] | i in 1..n], values)

;


output
[
  "x: " ++ show(x) ++ "\n" ++ 
  "xs: " ++ show(xs) ++ "\n"
];