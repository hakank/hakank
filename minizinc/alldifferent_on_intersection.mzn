% 
% Global constraint alldifferent_on_intersection in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Calldifferent_on_intersection.html
% """
% The values that both occur in the VARIABLES1 and VARIABLES2 collections 
% have only one occurrence.
% 
% Example
% (
%  <5, 9, 1, 5>,
%  <2, 1, 6, 9, 6, 2>
% )
% 
% The alldifferent_on_intersection constraint holds since the values 9 and 1 
% that both occur in <5, 9, 1, 5> as well as in <2, 1, 6, 9, 6, 2> have 
% exactly one occurrence in each collection.
% """


% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: m = 4;
int: n = 6;

array[1..m] of var 1..9: x;
array[1..n] of var 1..9: y;

%
% alldifferent_on_intersection
%
predicate alldifferent_on_intersection(array[int] of var int: x, array[int] of var int: y) =
  let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
     int: lby = min(index_set(y)),
     int: uby = max(index_set(y)),
  }
  in
  forall(i in lbx..ubx) (
     sum(j in lby..uby) (bool2int(x[i] = y[j])) <= 1
  )
  /\
  forall(j in lby..uby) (
     sum(i in lbx..lby) (bool2int(x[i] = y[j])) <= 1
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

constraint
  cp1d([5,9,1,5],x)
  /\
  cp1d([2,1,6,9,6,2],y) % constraint holds
  % cp1d([2,1,6,9,6,1],y) % constraint do not hold since there are two 1's in 1 and one 1 in x
  /\
  alldifferent_on_intersection(x,y)
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n"
];
