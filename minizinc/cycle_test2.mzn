%
% Implementation of the global constraint cycle in Minizinc
%
% This Minizinc program is written by Hakan Kjellerstrand, hakank@bonetmail.com,
% and is commented in the (swedish) blog post
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";


int: n = 6;
array[1..n] of var 1..n: x;
%array[1..n, 1..n] of var 1..n: y;
% array[1..n, 1..n] of var 1..n: y_transposed;

var int: num_cycles;


%
% This is quite complicated, but probably simpler to implement than using sets.
% The principle is simple:
% - create a cycle matrix from the permutation
% - create first fix position per column
% - count number of position (global_cardinality)
% - sum over that amount (gcc[i] / i)
%
predicate cycle(var int: nc, array[int] of var int: x) = 
 let {
    int: n = length(x),
    int: lbx = min(index_set(x)),
    int: ubx = max(index_set(x)),
    array[lbx..ubx] of var lbx..ubx: ff_pos,
    array[lbx..ubx] of var 0..ubx: gcc_card,
    array[lbx..ubx, lbx..ubx] of var lbx..ubx: y
 } 
 in
 cycle_matrix(x,y)
 /\
 forall(i,j in lbx..ubx) (
   let {
       % note: it is the _column_ j we are interested in, not the row i,
       % hence the construct index[j,i] 
        array[lbx..ubx] of var lbx..ubx: tmp = [ y[j,i] | j in lbx..ubx]
    } 
    in
    first_fixed(tmp, i, ff_pos[i])
  )
  /\ 
  global_cardinality(ff_pos, set2array(index_set(gcc_card)),  gcc_card)
  /\
  num_cycles = sum(i in lbx..ubx) (
                  gcc_card[i] div i
               )            
;


% find first fixed position in an array
predicate first_fixed(array[int] of var int: x, var int: v, var int: ix) =
   let {
       int: n = length(x),
       int: lbx = min(index_set(x)),
       int: ubx = max(index_set(x)),
   }
   in
   forall(i in lbx..ubx) (
       % 1) ix must be i
       % 2) and the must be not other i before this position
       ix = i <-> (x[i] = v /\
                   forall(j in lbx..i-1) (
                       x[j] != v
                   )
                  )
  )
;

predicate cycle_matrix(array[int] of var int: x, array[int,int] of var int: m) =
  let { 
     int: n = length(x) ,
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),     
  } 
  in
  all_different(x) /\  
  % copy array x to first line in matrix
  forall(j in lbx..ubx) (
     m[lbx,j] = x[j]
  ) /\
  % create the rotations in the following rows
  % note that the values is from x (first row in m)
   forall(i in lbx+1..ubx, j in lbx..ubx) (
      m[i,j] = m[i-1,m[1,j]]
  ) 
;

solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
  all_different(x) /\
  cycle(num_cycles, x)
  % some constraints...
  /\ num_cycles >= 0
  /\ num_cycles = 4

;


output [
   "x: ", show(x), " num_cycles: ", show(num_cycles) ++ "\n"
] 


