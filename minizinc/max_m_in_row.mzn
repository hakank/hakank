% 
% Max m in row in MiniZinc.
% 
% This constraint ensures that there are max m consecutive values
% of the same element in a row.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4; % dimensions of matrix
int: m = 2; % there must be max m of the same value consecutive in a row

array[1..n, 1..n] of var 0..2: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

%
% Ensure that there is atmost m consecutive entries of the same value
% in a row.
% 
predicate max_m_in_row(array[int] of var int: a,
                       int: m) =
   let {
       int: lbb = lb_array(a),
       int: ubb = ub_array(a),
       int: start = min(index_set(a)),
       int: end = max(index_set(a))
   } in
   forall(i in start..end-m) (
      let {
        array[lbb..ubb] of var 0..m: gcc
      } in
      global_cardinality([a[i+k] | k in 0..m], array1d(lbb..ubb, set2array(index_set(gcc))), gcc) :: domain
  )   
;


constraint
   forall(i in 1..n) (
       max_m_in_row([x[i,j] | j in 1..n], m) /\
       max_m_in_row([x[j,i] | j in 1..n], m) 
   )
;

output [
  if j = 1 then "\n" else " " endif ++
   show_int(2, x[i,j])
  | i,j in 1..n
]
 ++ ["\n"]
;

