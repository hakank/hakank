% 
% Ormat game problem instance in MiniZinc.
%
% From bit-player "The Ormat Game"
% http://bit-player.org/2010/the-ormat-game
%
% See the following for more information about the Ormat game model:
% - http://www.hakank.org/minizinc/ormat_game.mzn 
% - http://www.hakank.org/minizinc/ormat_game.mzn 
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Problem 3 (n=3)
%
include "ormat_game_mip_model.mzn";
include "ormat_game_n3.dzn";
problem = array2d(1..n, 1..n,
[
 1,1,1,
 1,1,1,
 1,1,0
]);
