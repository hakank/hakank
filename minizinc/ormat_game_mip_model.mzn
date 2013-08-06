% 
% Ormat game in MiniZinc.
%
% This is a MIP variant.
% 
% See http://www.hakank.org/minizinc/ormat_game.mzn
% for a complete MiniZinc model.
%
% This file is to be included in a problem file, see
%   http://www.hakank.org/minizinc/
%
% From bit-player "The Ormat Game"
% http://bit-player.org/2010/the-ormat-game
% """
% I'm going to give you a square grid, with some of the cells colored 
% and others possibly left blank. We'll call this a template. Perhaps 
% the grid will be one of these 3x3 templates:
% 
% [see pictures at the web page]
% 
% You have a supply of transparent plastic overlays that match the 
% grid in size and shape and that also bear patterns of black dots:
% 
% [ibid.]
%
% Your task is to assemble a subset of the overlays and lay them on 
% the template in such a way that dots cover all the colored squares 
% but none of the blank squares. You are welcome to superimpose multiple 
% dots on any colored square, but overall you want to use as few overlays 
% as possible. To make things interesting, I'll suggest a wager. I'll pay 
% you $3 for a correct covering of a 3x3 template, but you have to pay me 
% $1 for each overlay you use. Is this a good bet?
% """
% 
% 

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n;

array[1..n, 1..n] of 0..1: problem;
int: f = product([i | i in 1..n]);

% Which overlay to use.
array[1..f] of var 0..1: x;

% number of overlays used (to minimize)
var 1..f: num_overlays = sum(x);

% There are n! possible overlays
% They are in a separate .dzn file
%    ormat_game_n3.dzn etc
array[1..f, 1..n,1..n] of 0..1: overlays;

% solve satisfy;
% solve minimize num_overlays;
solve :: int_search(x ++ [num_overlays], smallest, indomain_min, complete) minimize num_overlays;


constraint 
    forall(i,j in 1..n) (
        % if problem has a black cell (=1) then there must be a selected
        % overlay that has a black cell
        if problem[i,j] = 1 then
            sum(o in 1..f) (x[o]*overlays[o,i,j]) >= 1
        else 
            sum(o in 1..f) (x[o]*overlays[o,i,j]) == 0
        endif
    )
;

%
% To be used with the minizinc solver showing
% all the used overlays etc.
%
output 
[
  "Problem:"
] 
++
[
   if j = 1 then "\n" else "" endif ++
      show(problem[i,j])
   | i,j in 1..n
] 
++
[
  "\n\nSolution:\nx: ", show(x), "\n",
  %"overlays_used: ", show(overlays_used), "\n",
  "num_overlays: ", show(num_overlays), "\n"
] ++
[
  if fix(x[o]) = 1 /\ j = 1 then "\n" else "" endif ++
  if fix(x[o]) = 1 /\ i = 1 /\ j = 1 then "Overlay #" ++ show(o) ++ "\n" else "" endif ++
  if fix(x[o]) = 1 then show(overlays[o, i,j]) else "" endif ++
  if fix(x[o]) = 1 /\ i = n /\ j = n then "\n" else "" endif

  | o in 1..f, i,j in 1..n
] 
++ 
["\n"];
