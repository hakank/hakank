% Simple integer programming problem in Minizinc
%
% From Applications of Optimization with XPress-MP.pdf
% page 11. The problem is presented on page 7.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

var int: smallset;
var int: largeset;

solve maximize 5*smallset + 20*largeset;

constraint
  smallset >= 0 /\ largeset >= 0 
  /\
  1*smallset + 3*largeset <= 200
  /\
  3*smallset + 2*largeset <= 160
;

output
[
  "smallset: " ++ show(smallset) ++ "\n" ++
  "largeset: " ++ show(largeset) ++ "\n"
];