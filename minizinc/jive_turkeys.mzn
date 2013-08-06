% 
% Jive turkeys puzzle in MiniZinc.

% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 5. 
% Description  : Jive turkeys
% Source       : rec.puzzles

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s5.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
array[1..3] of var int: x; % >= 0;

solve minimize x[3]; 
constraint
   forall(i in 1..3) (
     x[i] >= 0
   )
   /\
   10000*x[1] + 6790 + x[2] = 72*x[3]
   /\
   forall(i in 1..2) (
     x[i] <= 9
   )
   /\
   x[3] <= 1345
;

output
[
  show(x)
];
