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


% 
% Oleg German, Evgeny Lakshtanov: "Minesweeper" without a computer
% http://arxiv.org/abs/0806.3480, page 4
r = 5;
c = 6;
game = array2d(1..r, 1..c, [
    X,1,X,1,X,1,
    2,X,2,X,1,X,
    X,3,X,2,X,1,
    1,X,3,X,2,X,
    X,1,X,2,X,1,
]);

