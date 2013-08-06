%
% Minesweeper in MiniZinc.
%
% See http://www.hakank.org/minizinc/minesweeper_model.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "minesweeper_model.mzn";

% From "Some Minesweeper Configurations",page 3
% 4 solutions
r=8;
c=8;
game = array2d(1..r,1..c,[
   2,3,X,2,2,X,2,1,
   X,X,4,X,X,4,X,2,
   X,X,X,X,X,X,4,X,
   X,5,X,6,X,X,X,2,
   2,X,X,X,5,5,X,2,
   1,3,4,X,X,X,4,X,
   0,1,X,4,X,X,X,3,
   0,1,2,X,2,3,X,2 
]);

