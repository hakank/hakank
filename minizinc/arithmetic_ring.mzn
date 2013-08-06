% 
% Arithmetic Ring problem in MiniZinc.
% 
% From "Star problems"
% http://mathschallenge.net/problems/pdfs/mathschallenge_1_star.pdf
% page 6,
% """
% Problem
% The digits 1, 2, 3, 4, 5, 6, 7 and 8 are placed in the ring below.
%    1 5 3
%    8 _ 7
%    4 6 2
%
% With the exception of 6 and 7, no two adjacent numbers are consecutive.
% Show how it is possible to arrange the digits 1 to 8 in the ring so 
% that no two adjacent numbers are consecutive.
% """

% Note: I interpret this as 6 and 7 _must_ be adjacent.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 8;

% decision variables
array[1..n] of var 1..n: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


predicate check(var int: a, var int: b) =
  (
   (
      not(a = 6 /\ b = 7)
      /\
      not(a = 7 /\ b = 6)
   ) -> abs(a-b) > 1
  )
;

constraint
  alldifferent(x)
  /\

  % no consecutive number (except 6 and 7) can be adjacent
  forall(i in 2..n) (
     check(x[i-1], x[i]) 
  )
  /\ % around the corner
  check(x[n], x[1]) 

  /\ % however, 6 and 7 must be adjacent
     % (it cannot be "around the corner" since x[1] = 1)
  exists(i in 2..n-1) (
     (x[i] = 6 /\ x[i+1] = 7)
     \/
     (x[i] = 7 /\ x[i+1] = 6)
  )

  /\ % symmetry breaking
  x[1] = 1
;


output [
  show(x) ++ "\n"
]
++
[ % Show a nice solution for n=8 only.
  if n = 8 then
    show([x[1],x[2],x[3]]) ++ "\n" ++
    "[" ++ show(x[8]) ++ "     " ++ show(x[4]) ++ "]\n" ++
    show([x[7],x[6],x[5]])
  else 
    ""
  endif
];

