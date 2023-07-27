% 
% Global constraint alldifferent_interval in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Calldifferent_interval.html
% """
% 
% Enforce all variables of the collection VARIABLES to belong to 
% distinct intervals. The intervals are defined by 
%   [SIZE_INTERVAL*k, SIZE_INTERVAL*k+SIZE_INTERVAL-1] where k is an integer.
% 
% Example
%     ​(〈2,​3,​10〉,​3)​
% 
% In the example, the second argument SIZE_INTERVAL=3 defines the 
% following family of intervals [3*k, 3*k+2], where k is an integer. 
% Since the three variables of the collection VARIABLES take values that 
% are respectively located within the three following distinct intervals 
% [0, 2], [3, 5] and [9, 11], the alldifferent_interval constraint holds.
% """
 

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 3;
array[1..n] of var 1..10: x;
array[1..n, 1..2] of var 0..10*2: intervals;
var 1..9: size_interval;


%
% alldifferent_interval(VARIABLES, SIZE_INTERVAL, INTERVALS)
%
predicate alldifferent_interval(array[int] of var int: variables, 
                                var int: size_interval, 
                                array[int, int] of var int: intervals) =
  let {
     int: lbx = min(index_set(variables)),
     int: ubx = max(index_set(variables))
  } 
  in
  all_different(variables)

  /\ % interval[i] > interval[i-1]
   forall(i in lbx+1..ubx) (  
     intervals[i,1] > intervals[i-1,2]
  )
  /\ % assure the size of interval 
     % and check that x[i] is in the i'th interval
  forall(i in lbx..ubx) (  
    intervals[i, 2] - intervals[i, 1] = size_interval - 1
    /\
    variables[i] >= intervals[i, 1]
    /\
    variables[i] <= intervals[i, 2]      
  )
  /\ % get the k
  exists(k in lbx..ubx) (
    intervals[k, 1] = size_interval*k
    /\
    intervals[k, 2] = size_interval*k+size_interval-1
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


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  cp1d([2, 3, 10],x)
  /\
  alldifferent_interval(x, size_interval, intervals)
  %/\ 
  %size_interval = 3
;


output [
  "x: " ++ show(x) ++ "\n" ++
  "size_interval: " ++ show(size_interval) ++ "\n" ++
  "intervals: " ++ show(intervals) ++ "\n"
];
