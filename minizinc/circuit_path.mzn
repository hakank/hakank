%
% Circuit path in Minizinc
%
% This constraint extracts the path from a circuit constraint.
% 
% The Global Constraint Catalog for circuit:
%     http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
 

include "globals.mzn";
int: n = 5;

array[1..n] of var 1..n: x;
% the path to extract
array[1..n] of var 1..n: p;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


%
% This variant uses an extra array (z) for the orbit of x[1].
%
% The constraint is that z[i] must not be 1 until i = n and then
% must be 1.
%
predicate mycircuit(array[int] of var int: x) =
  let { 
        int: lbx = min(index_set(x)),
        int: ubx = max(index_set(x)),
        array[lbx..ubx] of var lbx..ubx: z
  }
  in
   all_different(x) /\
   all_different(z) /\

   % put the orbit of x[1] in in z[1..n]
   % 
   z[lbx] = x[lbx] /\ 
   forall(i in lbx+1..ubx) (
      z[i] = x[z[i-1]]
   )
   /\ % may not be 1 for i < n
   forall(i in lbx..ubx-1) (
      z[i] != lbx
   )
   /\  % when i = n it must be 1
   z[n] = lbx
;


predicate circuit_path(array[int] of var int: x, 
                       array[int] of var int: p) =
  let {
    int: len = length(x)
  } in
  % we always starts the path at 1
  p[1] = x[1]
  /\
  p[len] = 1
  /\
  forall(i in 2..len) (
    p[i] = x[p[i-1]] 
  )

; 

constraint
  mycircuit(x)
  % circuit(x)
  /\
  circuit_path(x, p)
;
output [
      show(x), "\n" ++
      show(p), "\n"
%       "z: ", show(z), "\n"
];
