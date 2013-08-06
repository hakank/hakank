% 
% Werewolfes II (Smullyan) puzzle in MiniZinc.
% 
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles3.html, puzzle nr. 9.
% Description  : Werewolves II
% Source       : Smullyan, R., (1978), What is the Name of this Book?, Prentice-Hall

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol3s9.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: person = 3;
array[1..person] of var 0..1: x; % x(i) = 1 if person i is a knight, 0 if a knave
array[1..person] of var 0..1: y; % y(i) = 1 if person i is a werewolf, 0 otherwise

var int: z = sum(i in 1..person) (x[i]+9*x[3]);

solve satisfy;

constraint
   % only one is a werewolf
   sum(i in 1..person) (y[i]) = 1 

   /\ % if statement 1 is true then set x(1) = 1, else 0
   y[1] - 9*x[1] <= 0 
   /\ 
   y[1] - x[1] >= 0 

   /\ % if statement 2 is true then set x(2) = 1, else 0
   y[2] - 9*x[2] <= 0 
   /\ 
   y[2] - x[2] >= 0 

   /\ % if statement 3 is true then set x(3) = 1, else 0
   sum(i in 1..person) (x[i]+9*x[3]) >= 2 
   /\ 
   % sum(i in 1..person) (x[i]+9*x[3]) <= 10 % original. This don't work!
   sum(i in 1..person) (x[i]+9*x[3]) <= 28 % this works...
;

output [
  "x: ", show(x), "\n",
  "y: ", show(y), "\n",
  "z: ", show(z), "\n"

];