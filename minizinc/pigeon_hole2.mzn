% 
% Pigeon hole problem in MiniZinc. 
%
% Alternative version, cf pigeon_hole.mzn .
%
% ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
% """
% pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes (at most 1 pigeon per hole). The boolean formulation uses n × m variables to indicate, for each pigeon, its hole number. Obviously, there is a solution iff n <= m.
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: N = 3; % N pigeons
int: M = 14; % M pigeon holes
array[1..M] of var 0..N: pigeon_holes;

% every integer > 0 represents an pigeon
predicate pigeon(array[int] of var int: p, int: n) =
  forall(i in 1..n) (
     count(p, i, 1)
  )
;

solve satisfy;

constraint
  pigeon(pigeon_holes, N)
;

output [
  show(pigeon_holes), "\n"

];
