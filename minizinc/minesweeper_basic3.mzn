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



% Basic position
r = 2;
c = 3;
game = array2d(1..r, 1..c, [
     X,X,X,
     1,1,1   
]);

