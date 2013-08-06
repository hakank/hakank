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

% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
%
% A Wire,page 33
% (2 solutions)
r = 5;
c = 14;
game = array2d(1..r,1..c,[
    X,0,0,0,0,0,0,0,0,0,0,0,0,X,
    X,1,1,1,1,1,1,1,1,1,1,1,1,X,
    X,X,1,X,X,1,X,X,1,X,X,1,X,X,
    X,1,1,1,1,1,1,1,1,1,1,1,1,X,
    X,0,0,0,0,0,0,0,0,0,0,0,0,X,
]);

