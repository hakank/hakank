% 
% Global constraint bin_packing in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cbin_packing.html
% """
% Given several items of the collection ITEMS (each of them having a specific 
% weight), and different bins of a fixed capacity, assign each item to a bin 
% so that the total weight of the items in each bin does not exceed CAPACITY.
% 
% Example
%   <(5,<bin-3 weight-4, bin-1 weight-3,bin-3 weight-1>)>
% 
%  The bin_packing constraint holds since the sum of the height of items 
% that are assigned to bins 1 and 3 is respectively equal to 3 and 5. 
% The previous quantities are both less than or equal to the maximum 
% CAPACITY 5. Figure 4.35.1 shows the solution associated with the example.
% 
% Remark
% 
% Note the difference with the classical bin-packing problem [MT90] where 
% one wants to find solutions that minimise the number of bins. In our 
% case each item may be assigned only to specific bins (i.e., the different 
% values of the bin variable) and the goal is to find a feasible solution. 
% This constraint can be seen as a special case of the cumulative 
% constraint [AB93], where all task durations are equal to 1.
% """

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 3;
array[1..n] of var 1..n: bins;
array[1..n] of var 1..4: weights;
var int: capacity;

solve satisfy;


%
% bin_packing2
% (The name is bin_packing2 since I already have a predicate bin_packing.
%  Sorry about any confusion.)
% 
predicate bin_packing2(var int: capacity, array[int] of var int: bins, array[int] of var int: weights) =

  forall(b in index_set(bins)) (
     sum(j in index_set(bins)) ( weights[j]*bool2int(bins[j] = b)) <= capacity
  )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


constraint
  % cp1d([3,1,3], bins)
  %/\
  cp1d([4,3,1],weights)
  /\
  capacity = 5
  /\
  bin_packing2(capacity, bins, weights)

;

output
[ 
  "bins: " ++ show(bins) ++ "\n" ++
  "weights: " ++ show(weights) ++ "\n"
];
