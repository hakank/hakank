%
% Defending the Castle puzzle in Minizinc
%
%
% From http://www.cut-the-knot.org/do_you_know/lin_pr.shtml
% """
% Assume that we have decided to position defenders of a square castle according to the following plan:

% p q p
% q 0 q
% p q p

% so that the total number of defenders is 4(p+q) while (2p+q) fighters face 
% the enemy on every side. Let's denote p = x1 and q = x2. Let also A = (4 4), 
% be a 1x2 matrix, and c = (2 1), a 1x2 row vector. Assume that a 1x1 vector 
% b is equal to (28). The linear programming problem (P) is then interpreted 
% as:
% How to position 28 fighters according to the symmetric arrangement above 
% so as to have the maximum number of defenders on each side?
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

var int: p; % >= 0 integer;
var int: q; % >= 0 integer;
var int: z = 2*p + q; 

solve maximize z;
% solve satisfy;

constraint
%  z = 10
%  /\
  p >= 0 /\ q >= 0
  /\
  4*(p + q) = 28
  /\
  q >= p
;

output [
    "z: ", show(z), "\n",
    show(p), " ", show(q), " ", show(p), "\n",
    show(q), " ", "0 ", show(q), "\n",
    show(p), " ", show(q), " ", show(p), "\n"
];
