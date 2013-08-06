%
% Minesweeper in MiniZinc.
%
% Problem 8. See
% http://www.hakank.org/minizinc/minesweeper_model.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "minesweeper_model.mzn";

% Problem from Gecode/examples/minesweeper.cc  problem 8
r=7;
c=7;
game = array2d(1..r,1..c,[
       X,X,X,X,X,X,X,
       X,2,3,4,3,5,X,
       X,1,X,X,X,3,X,
       X,X,X,5,X,X,X,
       X,1,X,X,X,3,X,
       X,1,2,2,3,4,X,
       X,X,X,X,X,X,X,
]);

