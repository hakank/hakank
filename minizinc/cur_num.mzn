%
% Puzzle in Minizinc
%
% From Martin Henz' collection of puzzles
% http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/#curious
% """
%   Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.
%
%   The number 48 has this peculiarity, that if you add 1 to it the result
%   is a square number, and if you add 1 to its half, you also get a
%   square number. Now, there is no limit to the numbers that have this
%   peculiarity, and it is an interesting puzzle to find three more of
%   them---the smallest possible numbers. What are they?
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: up = 4000000; % upper range limit
var 1..up: X;
var 1..up: A;
var 1..up: B;
var 1..up: C;
var 1..up: D;
var 1..up: E;
array[1..6] of var 1..up: arr = [X,A,B,C,D,E]; 


constraint 
  X + 1 = A    % if you add 1 to it
  /\ A = B * B % the result is a square number
  /\ X = 2 * C % if you to its half
  /\ C + 1 = D % add 1
  /\ D = E * E % you also get a square number
;

solve satisfy;
% solve :: int_search(arr,"first_fail","indomain", "credit(5,bbs(5))") satisfy;

output [
       show(arr), "\n",
];
