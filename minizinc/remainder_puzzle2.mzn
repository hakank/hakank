% 
% Remainder puzzle in MiniZinc.
%
% """
% 11.  Is there a number which when divided by 3 gives a remainder of 1;
% when divided by 4, gives a remainder of 2; when divided by 5, gives a
% remainder of 3; and when divided by 6, gives a remainder of 4?
% (Kordemsky)
% """
%
% Also, compare with the following models:
% * MiniZinc: http://www.hakank.org/minizinc/remainder_puzzle.mzn
%
% * ECLiPSe: http://www.hakank.org/eclipse/remainders.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/remainders.pl
% * Gecode: http://www.hakank.org/gecode/remainders.cpp

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
set of int: R = 1..10000;
%var R: X;
%var R: A;
%var R: B;
%var R: C;
%var R: D;

array[1..5] of var R: x;

solve satisfy;

constraint

  %X == A*3 + 1 /\
  %X == B*4 + 2 /\
  %X == C*5 + 3 /\
  %X == D*6 + 4

  % more general
  forall(i in 2..5) (
     x[1] = x[i]*(i+1) + i-1
  )

;

output [
"x: " ++ show(x) ++ "\n"

];
