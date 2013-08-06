% 
% Ormat game in MiniZinc.
%
% This is for generating all the different overlays for a specific
% n x n grid. 
% 
% For more about Ormat game, see http://www.hakank.org/minizinc/ormat_game.mzn
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;

int: f = product([i | i in 1..n]);

% there are n! possible overlays
% (which should be dynamically generated)
% array[1..f, 1..n,1..n] of var 0..1: overlays;

array[1..n,1..n] of var 0..1: overlays;

solve satisfy;

% % Generating all the overlays (but this requires multiple solutions)
constraint
   forall(i in 1..n) (
        sum([overlays[i,j] | j in 1..n]) == 1 /\
        sum([overlays[j,i] | j in 1..n]) == 1 
   )
;


output 
[
  if j = 1 then "\n" else "" endif ++
  show(overlays[i,j]) ++ ", "
  | i,j in 1..n
] ++ 
["\n"];