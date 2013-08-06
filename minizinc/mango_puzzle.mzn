% 
% Magno puzzle in MiniZinc.
% 
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 9.
% Description  : Mango puzzle
% Source       : M Kraitchik - Mathematical Recreations (P32)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s9.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: n = 5;
array[1..n] of var int: x;

constraint 
      % integer version
      forall(i in 1..n) ( x[i] >= 0) /\
      -3*x[1]+x[2] = 1 /\
      -3*x[2]+2*x[3] = 2 /\  
      -3*x[3]+2*x[4] = 2 /\  % multiplies by 2 for integer version
      -3*x[4]+2*x[5] = 2     % ibid

;

solve minimize x[n];

output [
  "x: ", show(x), "\n"

];
