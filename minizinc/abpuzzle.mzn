%  
% The Abbott's Puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles3.html  puzzle nr. 1.  
% Description  : The Abbott's Puzzle
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol3s1.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of int: S = 1..3;
% array[S] of 0.0..3.0: amount = [3.0, 2.0, 0.5]; % float version
array[S] of 0..6: amount = [6, 4, 1]; % multiply with 2 for the integer solution
array[S] of var 0..1000: x;

solve satisfy;

constraint
   % float version
   % sum(i in S) (amount[i]*int2float(x[i])) = 100.0
   % /\
   % sum(i in S) (x[i]) = 100
   % /\
   % x[2] = 5*x[1]

   % integer version
   sum(i in S) (amount[i]*x[i]) = 200
   /\
   sum(i in S) (x[i]) = 100
   /\
   x[2] = 5*x[1]
;

output [
  show(x), "\n",
];