%
% Minesweeper in MiniZinc.
%
% Problem 6. See 
% http://www.hakank.org/minizinc/minesweeper_model.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "minesweeper_model.mzn";

% Problem from Gecode/examples/minesweeper.cc  problem 6
r = 8;
c = 8;
game = array2d(1..r,1..c,[
       X,3,2,X,X,1,X,X,
       X,X,X,X,1,X,X,3,
       3,X,X,2,X,X,X,4,
       X,5,X,X,X,5,X,X,
       X,X,6,X,X,X,5,X,
       3,X,X,X,5,X,X,4,
       2,X,X,5,X,X,X,X,
       X,X,2,X,X,3,4,X,
]);

