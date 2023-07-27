%
% Implementation of the global constraint circuit in Minizinc
%
% Cf https://sofdem.github.io/gccat/gccat/Ccircuit.html
%
% This Minizinc program is written by Hakan Kjellerstrand and is commented in 
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
 

include "globals.mzn";
int: n = 6;
array[1..n] of var 1..n: x;


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


constraint
  % mycircuit(x)
  circuit(x)
;
output [
      show(x), "\n",
%       "z: ", show(z), "\n"
];
