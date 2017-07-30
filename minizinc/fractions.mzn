% 
% Fractions problem in MiniZinc.
%
% Prolog benchmark problem (BProlog)
% """
% Find distinct non-zero digits such that the following equation holds:
%        A        D        G
%     ------  + ----- + ------  = 1
%       B*C      E*F      H*I
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Solution: 
% [9, 1, 2, 5, 3, 4, 7, 6, 8]


include "globals.mzn"; 

var 1..9: A;
var 1..9: B;
var 1..9: C;
var 1..9: D;
var 1..9: E;
var 1..9: F;
var 1..9: G;
var 1..9: H;
var 1..9: I;
array[1..9] of var 1..9: Vars=[A,B,C,D,E,F,G,H,I];

var 1..81: D1;
var 1..81: D2;
var 1..81: D3;



% solve satisfy;
solve :: int_search(Vars ++ [D1,D2,D3], first_fail, indomain_min, complete) satisfy;

constraint
   all_different(Vars) /\
   D1 = 10*B+C /\
   D2 = 10*E+F /\
   D3 = 10*H+I /\
   A*D2*D3 + D*D1*D3 + G*D1*D2 = D1*D2*D3 /\
   % break the symmetry
   A*D2 >= D*D1 /\
   D*D3 >= G*D2 /\
   %redundant constraints
   3*A >= D1 /\
   3*G <= D2

;

output [
  show(Vars), "\n"
]
++
[
  "\n" ++ 
  show(A) ++ "/(" ++ show(B) ++ show(C) ++") + " ++
  show(D) ++ "/(" ++ show(E) ++ show(F) ++") + " ++
  show(G) ++ "/(" ++ show(H) ++ show(I) ++") = 1\n "

]
;