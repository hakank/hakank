% 
% Book buy puzzle in MiniZinc.
% 
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 9.
% Source:  M Kraitchik, Mathematical Recreations(p37), Dover
%
% This model was inspired by the AMPL model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s9.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% fathers: 1 = Peter, 2 = Paul, 
% sons: 1 = Tom, 2 = Dick
int: m = 2;   
set of 1..m: M = 1..m;

% w = 1 if Peter is Tom's father, 0 otherwise 
var 0..1: w;           

% number of books (and price) bought by father i
array[M] of var 1..8: x;
% number of books (and price) bought by son j
array[M] of var 1..8: y;

solve satisfy;

constraint
   y[2] = 1      % Dick buys one book
   /\
   x[1] = y[1]+1 % Peter buys one more book than Tom
   /\
   % each family spends $65
   x[1]*x[1] + w*y[1]*y[1] + (1-w)*y[2]*y[2] = 65
   /\
   x[2]*x[2] + (1-w)*y[1]*y[1] + w*y[2]*y[2] = 65
;


output
[
  "w: " ++ show(w) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" 

];