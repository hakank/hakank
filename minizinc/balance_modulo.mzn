% 
% Global constraint balance_modulo in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Cbalance_modulo.html
% """
%Constraint
%
% balance_modulo(BALANCE, VARIABLES, M)
%
% Purpose
%
% Consider the largest set S1 (respectively the smallest set S2) of variables 
% of the collection VARIABLES that have the same remainder when 
% divided by M. BALANCE is equal to the difference between the cardinality 
% of S2 and the cardinality of S1.
%
% Example
% (2, <6, 1, 7, 1, 5>, 3)
%
% In this example values 6, 1, 7, 1, 5 are respectively associated with 
% the equivalence classes 6 mod 3=0, 1 mod 3=1, 7 mod 3=1, 1 mod 3=1, 
% 5 mod 3=2. Therefore the equivalence classes 0, 1 and 2 are respectively 
% used 1, 3 and 1 times. The balance_modulo constraint holds since its 
% first argument BALANCE is assigned to the difference between the maximum 
% and minimum number of the previous occurrences (i.e., 3-1).
%
% Usage
%
% An application of the balance_modulo constraint is to enforce a balanced 
% assignment of values, no matter how many distinct equivalence classes 
% will be used. In this case one will push down the maximum value of 
% the first argument of the balance_modulo constraint.
% """
%

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..7: x;
var 0..n: bal;
int: m = 3;

predicate min_except_0(var int: min_val, array[int] of var int: x) =

     exists(i in index_set(x)) (
       min_val = x[i] /\ 
       forall(j in index_set(x) where i != j) (
         x[i] <= x[j]
         \/ % ignore 0
         x[j] = 0
       )
     )
     /\
     min_val > 0
;


predicate balance_modulo(var int: bal, array[int] of var int: x, int: m) =
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
     array[0..m-1] of var 0..ubx: counts,
     var 0..ubx: c_max,
     var 0..ubx: c_min
   }
   in
   forall(i in 0..m-1) (
      counts[i] = sum(j in index_set(x)) ( bool2int( (x[j] mod m) = i))
   )
   /\ 
   c_max = max(counts)
   /\
   min_except_0(c_min, counts)
   /\
   bal = c_max - c_min
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
  forall(i in index_set(x)) (
      x[i] = y[i]
  )
  )
; 


% solve :: int_search(x ++ [bal], first_fail, indomain_min, complete) satisfy;
solve satisfy;

constraint
  cp1d([6,1,7,1,5], x)
  /\
  balance_modulo(bal, x, m)
  %/\
  %bal = 2

;


output [
  "x: ", show(x), "\n",
  "bal: ", show(bal), "\n"
];
