%
% Minesweeper in MiniZinc.
%
% Problem 2. See 
% http://www.hakank.org/minizinc/minesweeper_model.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "minesweeper_model.mzn";

% Problem from Gecode/examples/minesweeper.cc  problem 2
r = 10;
c = 10;
game = array2d(1..r, 1..c, [
      1,X,X,2,X,2,X,2,X,X,
      X,3,2,X,X,X,4,X,X,1,
      X,X,X,1,3,X,X,X,4,X,
      3,X,1,X,X,X,3,X,X,X,
      X,2,1,X,1,X,X,3,X,2,
      X,3,X,2,X,X,2,X,1,X,
      2,X,X,3,2,X,X,2,X,X,
      X,3,X,X,X,3,2,X,X,3,
      X,X,3,X,3,3,X,X,X,X,
      X,2,X,2,X,X,X,2,2,X
      ]);
