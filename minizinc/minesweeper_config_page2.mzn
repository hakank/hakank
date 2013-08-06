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

% From "Some Minesweeper Configurations",page 2
r = 6;
c = 6;
game = array2d(1..r,1..c,[
     X,X,X,X,X,X,
     X,2,2,2,2,X,
     X,2,0,0,2,X,
     X,2,0,0,2,X,
     X,2,2,2,2,X,
     X,X,X,X,X,X,
]);

