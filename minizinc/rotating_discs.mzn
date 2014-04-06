% 
% Rotating disc problem in MiniZinc.
% 
% From Jean-Luis Lauriere's ALICE paper
% "A Language and a Program for Stating and Solving 
% Combinatorial Problems" (1978), page 84f:
% """
% Banerji proposed a discs problem: four concentric discs are 
% divided into eight valued zones; the question is to turn the 
% discs round so that each octant counts up to the same value of 12.
% """
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 8; % 8 discs
int: num_discs = 4;
array[1..num_discs, 1..n] of int: v; % the discs

% decision variables
array[1..num_discs] of var 0..n-1: rot; % the rotation of each disc


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   forall(j in 1..n) (  
      % Note: The ALICE paper has this construct but it don't work,
      %       presumably since the modulo operator don't work the 
      %       same way in MiniZinc as in ALICE.
      % sum([v[i,1+((j-rot[i]-1) mod n)] | i in 1..num_discs]) = 12

      sum([v[i,1+((rot[i]+j-1) mod n)] | i in 1..num_discs]) = 12
   )

   /\ % symmetry breaking
   rot[1] = 0
   
;

output [
  "x: " ++ show(rot) ++ "\n"
]
++
[
   "zone " ++ show(j) ++ ": " ++ show([v[i,1+((fix(rot[i]+j-1)) mod n)] | i in 1..num_discs]) ++ "\n"
   | j in 1..n
]
;

%
% Data, the discs
%
v = array2d(1..num_discs, 1..n,
  [
     2,1,3,4,2,5,1,3,
     3,2,3,4,1,3,4,5,
     3,4,5,3,3,2,2,1,
     5,1,5,3,4,3,2,4,
  ]);
