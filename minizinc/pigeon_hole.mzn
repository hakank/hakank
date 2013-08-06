% 
% Pigeon hole problem in MiniZinc. 

% ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
% """
% pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes (at most 1 pigeon per hole). The boolean formulation uses n × m variables to indicate, for each pigeon, its hole number. Obviously, there is a solution iff n <= m.
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: N = 2; % N pigeons
int: M = 10; % M pigeon holes

% i pigeon at j pigeon holes
array[1..N,1..M] of var 0..1: pigeon_holes_ip;

%
% integer programming version
%
predicate pigeon_ip(int: N, int: M, array[1..N, 1..M] of var 0..1: p) = 

   % max 1 pigeon per pigeon hole
   forall(j in 1..M) (
     sum([p[i,j] | i in 1..N]) <= 1
   )
   /\ % all pigeon must be placed and only at one hole
   forall(i in 1..N) (
     sum([p[i,j] | j in 1..M]) = 1
   )
;

solve satisfy;

constraint

  % integer programming version 
  pigeon_ip(N, M, pigeon_holes_ip)
  % /\ pigeon_holes_ip[1,1] = 1 % some symmetry breaking

;

output [
  if j = 1 then "\n" else " " endif ++
  show(pigeon_holes_ip[i,j]) ++ " "
  | i in 1..N, j in 1..M
] ++ ["\n"];
