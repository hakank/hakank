% 
% Sortedness in MiniZinc.
% 
% Inspired by the Comet code from
% From https://www.ads.tuwien.ac.at/wiki/images/archive/f/f4/20110617214403!Sortedness.co

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

% decision variables

array[1..n] of var 1..30: x;
array[1..n] of var 1..30: s;
array[1..n] of var 1..n: p;

%
% From https://www.ads.tuwien.ac.at/wiki/images/archive/f/f4/20110617214403!Sortedness.co
% """
% * This function enforces the sortedness constraint, i.e., a channeling
% * constraint between the arrays of integer variables x and s
% * s is the sorted version of x, and the array p is the permutation of the 
% * indices in x to obtain the ordered version.
% * Notice that this function works only for arrays x, s, and p with range 1..n
% * The function could be used in the solve/minimize statement as in the
% * following example.
% """
%
predicate sortedness(array[int] of var int: x, 
                     array[int] of var int: s,
                     array[int] of var int: p) =
   let {
     set of int: R = index_set(x),
     int: minval = min([lb(x[i]) | i in R]),
     int: maxval = max([ub(x[i]) | i in R]),
     array[minval..maxval] of var 0..card(R) : occ,
     array[minval..maxval] of var 0..card(R) : nbBefore
  } in 
  forall (i in minval..maxval) (
      occ[i] == (sum(j in R) (bool2int(x[j] == i)))
  )
  /\
  forall (i in minval..maxval) (
    occ[i] == (sum(j in R) (bool2int(s[j] == i)))
  )
  /\
  nbBefore[minval] == 0 
  /\
  forall (i in (minval + 1)..maxval) (
    nbBefore[i] == sum(j in minval..(i - 1)) (occ[j])
  )
  /\
  forall (i in R) (
    nbBefore[s[i]] < i
  )
  /\
  forall (i in R) (
    s[i] == x[p[i]]
  )
  % /\ % hakank: added this
  % increasing(s)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  sortedness(x, s, p)
  /\
  alldifferent(x)
  /\
  x[2] == 5
  /\
  s[4] == 10

  % Testing for a specific x
  /\ cp1d(x, array1d(1..n, [29, 5, 21, 27, 10, 8, 23, 12, 9, 11]))
;

output [
  "x: " ++ show(x) ++ "\n" ++
  "s: " ++ show(s) ++ "\n" ++
  "p: " ++ show(p) ++ "\n"
];

