% 
% Number the regions in MiniZinc.
% 
% From "The Inquisitive Problem Solver", problem 21, page 5.
% """
% The three lines in the figure divide the circle into seven regions.
% Can you arrange the numbers 1 to 7 in these regions so that for each 
% of the lines, the sums of the numbers on either side are all the same.
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 7;

% the regions of each lines
array[1..3, 1..2] of set of 1..n: lines;

% decision variables
array[1..n] of var 1..n: x; 

% solve satisfy;
solve :: int_search(x, smallest, indomain_min, complete) satisfy;


constraint
    % for safety
    forall(i in 1..3) (
      disjoint(lines[i,1], lines[i,2]) % sets should be disjoint
      /\
      card(lines[i,1] union lines[i,2]) = n % all numbers are covered
    )
    /\
    all_different(x) 
    /\ % general version
    forall(i in 1..3) (

      sum(j in lines[i,1]) (x[j]) = sum(j in lines[i,2]) (x[j])
    )
    /\
    x[1] = 1 % symmetry breaking
%     /\ % "talkative" version
%      x[1]+x[5]+x[6]+x[7] = x[2]+x[3]+x[4]
%      /\
%      x[1]+x[2]+x[6]      = x[3]+x[4]+x[5]+x[7]
%      /\
%      x[1]+x[2]+x[3]+x[7] = x[4]+x[5]+x[6]
;

lines = array2d(1..3, 1..2, 
[
  % side 1     side 2
    {1,5,6,7}, {2,3,4},
    {1,2,6}  , {3,4,5,7},
    {1,2,3,7}, {4,5,6}
]);


output[
  "x: ", show(x), "\n",
];

