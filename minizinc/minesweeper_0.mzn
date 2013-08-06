%
% Minesweeper in MiniZinc.
%
% Problem 1. See 
% http://www.hakank.org/minizinc/minesweeper_model.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "minesweeper_model.mzn";

% Problem from Gecode/examples/minesweeper.cc  problem 0
r = 6;
c = 6;
game = array2d(1..r, 1..c, [
    X,X,2,X,3,X,
    2,X,X,X,X,X,
    X,X,2,4,X,3,
    1,X,3,4,X,X,
    X,X,X,X,X,3,
    X,3,X,3,X,X,
    ]);

