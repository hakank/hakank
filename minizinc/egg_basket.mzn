% 
% Egg basked puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 3
% Description  : Egg basket puzzle
% Source       : Boris Kordemsky - The Moscow Puzzles (P136)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s3.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: max_n = 6; 
array[1..max_n] of var int: x;
var int: n;

solve minimize n;

constraint
   forall(i in 1..max_n) (
     x[i] >= 0
   )

   % /\
   % n > 0 /\
   % n = 2*x[1]+1 /\
   % n = 3*x[2]+1 /\
   % n = 4*x[3]+1 /\
   % n = 5*x[4]+1 /\
   % n = 6*x[5]+1 /\
   % n = 7*x[6]

   % generalized solution
   /\
   n >= 1 /\
   forall(i in 1..max_n) (
     if i < max_n then
       n = (i+1)*x[i]+1 
     else 
       n = (i+1)*x[i] 
     endif
   )
;


output [
  "n: ", show(n) ++ "\n" ++
  "x: ", show(x), "\n" 
];