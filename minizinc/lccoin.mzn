% 
% Lewis Carroll coin puzzle in MiniZinc.

% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 3.
% Description  : Lewis Carroll coin puzzle
% Source       : Wakeling, E., (1995), Rediscovered Lewis Carroll Puzzles, Dover.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s3.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: person = 3;
int: coin = 10;

set of 1..person: P = 1..person; % buyer, shopkepper, friend
set of 1..coin: C = 1..coin;
array[P] of int: requ;
array[C] of int: value;
array[C, P] of var 0..1: x;

solve satisfy;

constraint
 forall(j in P) (
        sum(i in C) (value[i]*x[i,j]) = requ[j]
 )
 /\
 forall(i in C) (
        sum(j in P) (x[i,j]) = 1
 )
;


requ = [63, 160, 85];
value = [120, 60,48,30,24,12,6,4,3,1];


output [
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i in C, j in P
] ++ ["\n"];