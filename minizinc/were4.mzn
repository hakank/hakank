% 
% Werewolfes IV (Smullyan) puzzle in MiniZinc.

% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles3.html, puzzle nr. 10.
% Description  : Werewolves IV
% Source       : Smullyan, R., (1978), What is the Name of this Book?, Prentice-Hall

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol3s10.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: person = 3;
array[1..person] of var 0..1: x; % x(i) = 1 if person i is a knight, 0 if a knave
array[1..person] of var 0..1: y; % y(i) = 1 if person i is a werewolf, 0 otherwise

var int: z = sum(i in 1..person) (x[i] + 3*x[1]);

solve satisfy;

constraint
  % if statement 1 is true then x[1) = 1, else 0
  sum(i in 1..person) (x[i] + 3*x[1]) >= 3
  /\
  % sum(i in 1..person) (x[i]+3*x[1]) <= 5 % original, don't work
  sum(i in 1..person) (x[i] + 3*x[1]) <= 10 % this works
  /\

  % if statement 2 is true then x[2) = 1, else 0
  x[3] = x[2]

  /\ % only one is a werewolf
  sum(i in 1..person) (y[i]) = 1

  /\ % werewolf is a knight
  forall(i in 1..person) (
    x[i] >= y[i]
  )
;



output [
  "x: ", show(x), "\n",
  "y: ", show(y), "\n",
  "z: ", show(z), "\n"

];
