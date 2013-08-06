%
% Minesweeper in MiniZinc.
%
% Problem 5. 
% See http://www.hakank.org/minizinc/minesweeper_model.mzn

%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "minesweeper_model.mzn";

% Problem from Gecode/examples/minesweeper.cc  problem 5
r = 10;
c = 10;
game = array2d(1..r,1..c,[
      X,2,1,X,2,X,2,X,X,X,
      X,4,X,X,3,X,X,X,5,3,
      X,X,X,4,X,4,4,X,X,3,
      4,X,4,X,X,5,X,6,X,X,
      X,X,4,5,X,X,X,X,5,4,
      3,4,X,X,X,X,5,5,X,X,
      X,X,4,X,4,X,X,5,X,5,
      2,X,X,3,3,X,6,X,X,X,
      3,6,X,X,X,3,X,X,4,X,
      X,X,X,4,X,2,X,2,1,X,
]);

