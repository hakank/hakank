% 
% Ormat game in MiniZinc.
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
% This is a prototype which the following limitations:
% - the overlays is not generated dynamically for each n
% - it just shows which overlays that is used. It would be nice
%   with a much more graphical output.
% - the questions asked by bit-player is not answered (it requires
%   more analysis)
%
% That said, here is solutions of the three problems stated at the web page
% with minimum number of overlays.
%    x is an indicator matrix which overlay to use
%    overlay used is a set representation of the overlay used
%    num_overlays is the number of overlays used
% 
% Problem 1 (unique optimal solution)
%     x: [1 1 0 0 0 0] 
%     overlays_used: 1..2
%     num_overlays: 2
%
% Problem 2 (two optimal solutions)
%     x: [1 0 0 1 1 0] 
%     overlays_used: 1,4,5
%     num_overlays: 3
% 
%     alternative solution
%     x: [0 1 1 0 0 1]
%     overlays_used = 2,3,6
%     num_overlays = 3
% 

% Problem 3 (unique optimal solution)
%     x: [0 1 0 1 1 1] 
%     overlays_used: 2,4,5,6
%     num_overlays: 4
% 
%
% The distribution of the number of problems for specific
% number of overlays used:
%   num_overlays  number of problems
%     0             1
%     1             6
%     2            15
%     3            20
%     4            15
%     5             6
%     6             1
%
% Which is the sequence binomial(6,1..6) 
%
% Problem 4: the problem stated for n = 4 (Problem 4 below)
%  1,1,1,1,
%  1,1,1,1,
%  1,1,1,1,
%  1,1,0,0
%
% has 46 solutions, one is:
%   num_overlays = 6;
%   overlays_used = {1, 4, 5, 7, 9, 10};
%   x: 1 0 0 1 1 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
%
% Problem 5 (n=5):
%  1,1,1,1,1,
%  1,1,1,1,1,
%  1,1,1,1,1,
%  1,1,1,1,1,
%  1,1,0,0,0
% 
% This has many optimal solutions. One is following solution
%   num_overlays = 8;
%   overlays_used = {6, 10, 15, 19, 25, 31, 33, 34};
%
%
% Note: This requires that a data file (.dzn) with overlays of 
%       the appropriate size is included.
%       E.g.  ormat_game_n3.mzn for n=3.
%        
% 
% Ah, by making a MIP problem it is quite fast.


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n;

int: f = product([i | i in 1..n]);

% Which overlay to use.
array[1..f] of var 0..1: x;
% var set of 1..f: overlays_used; % set representation

% number of overlays used (to minimize)
var 1..f: num_overlays = sum(x);

% for generating the problems (with solve satisfy)
% array[1..n, 1..n] of var 0..1: problem;

% There are n! possible overlays
% They are in a separate .dzn file
%    ormat_game_n3.dzn etc
array[1..f, 1..n,1..n] of 0..1: overlays;

solve satisfy;
% solve minimize num_overlays;
% solve :: int_search(x, smallest, indomain_min, complete) minimize num_overlays;

% constraint 
%     % num_overlays = 2 /\ % for problem 1 with solve satisfy
%     % num_overlays = 3 /\ % for problem 2 with solve satisfy
%     % num_overlays = 4 /\ % for problem 3 with solve satisfy

%     % num_overlays = 6 /\ % for problem 4 with solve satisfy
%     % num_overlays = 8 /\ % for problem 5 with solve satisfy

%     % if problem has a black cell (=1) then there must be a selected
%     % overlay that has a black cell
%     forall(i,j in 1..n) (
%         (
%         problem[i,j] = 1 -> exists(o in 1..f) (
%                                                 x[o]*overlays[o,i,j] = 1 
%                                               )
%         )
%         %/\
%         %(
%         %problem[i,j] = 0 -> forall(o in 1..f) (
%         %                                             x[o] = 1 -> overlays[o,i,j] = 0
%         %                                         )
%         %)
        
%     )

%     /\ % the inverse: wherever a selected overlay has a black cell, 
%        % problem must have a black cell
%     forall(o in 1..f) (
%         x[o] = 1 -> forall(i,j in 1..n) (
%                           overlays[o,i,j] = 1 -> problem[i,j] = 1
%                     ) 
%     ) 

%     %/\ 
%     %forall(i in 1..f) (
%     %   i in overlays_used <-> x[i] = 1
%     %)
% ;

% Let's make it a MIP problem instead.
constraint 
    num_overlays = 3 /\ % for problem 2 with solve satisfy
    forall(i,j in 1..n) (
        if problem[i,j] = 1 then
            sum(o in 1..f) (x[o]*overlays[o,i,j]) >= 1
        else 
            sum(o in 1..f) (x[o]*overlays[o,i,j]) == 0
        endif
    )
;


% Overlays for 3x3.
% Placed is the same order as the web page.
% 
% Note: this is now in the data file ormat_game_n3.mzn
%
% array[1..f, 1..n, 1..n] of 0..1: overlays = array3d(1..f,1..n,1..n,
% [
%   1, 0, 0, 
%   0, 1, 0, 
%   0, 0, 1,
%
%   1, 0, 0, 
%   0, 0, 1, 
%   0, 1, 0,
%
%   0, 1, 0, 
%   1, 0, 0, 
%   0, 0, 1,
%
%   0, 0, 1, 
%   1, 0, 0, 
%   0, 1, 0,
%
%   0, 1, 0, 
%   0, 0, 1, 
%   1, 0, 0,
%
%   0, 0, 1, 
%   0, 1, 0, 
%   1, 0, 0,
%
% ]);


% The following problems are from 
% http://bit-player.org/2010/the-ormat-game
%
% Problem grid 1
% include "ormat_game_n3.dzn";
% array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
% [
%   1,0,0,
%   0,1,1,
%   0,1,1
% ]);

% Problem grid 2
include "ormat_game_n3.dzn";
array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
[
  1,1,1,
  1,1,1,
  1,1,1
]);

% Problem grid 3
% include "ormat_game_n3.dzn";
% array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
% [
%   1,1,1,
%   1,1,1,
%   1,1,0
% ]);


% Problem grid 4 (n = 4)
% This is under the section "Out of bounds"
% include "ormat_game_n4.dzn";
% array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
% [
%   1,1,1,1,
%   1,1,1,1,
%   1,1,1,1,
%   1,1,0,0
% ]);


% Problem grid 5 (n = 5)
% This is under the section "Out of bounds"
% include "ormat_game_n5.dzn";
% array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
% [
%   1,1,1,1,1,
%   1,1,1,1,1,
%   1,1,1,1,1,
%   1,1,1,1,1,
%   1,1,0,0,0
% ]);

% 
% Problem grid 6 (n = 6)
% This is under the section "Out of bounds"%
% include "ormat_game_n6.dzn";
% array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
% [
%   1,1,1,1,1,1,
%   1,1,1,1,1,1,
%   1,1,1,1,1,1,
%   1,1,1,1,1,1,
%   1,1,1,1,1,1,
%   1,1,0,0,0,0
% ]);


% Problem grid 7 (n=7)
% Note: this is too large for mzn2fzn!
% include "ormat_game_n7.dzn";
% problem = array2d(1..n, 1..n,
% [
%  1,1,1,1,1,1,1,
%  1,1,1,1,1,1,1,
%  0,1,1,1,1,1,1,
%  0,0,1,1,1,1,1,
%  0,0,0,1,1,1,1,
%  0,0,0,0,1,1,1,
%  0,0,0,0,0,1,1
% ]);



% Misc problem
% array[1..n, 1..n] of 0..1: problem = array2d(1..n, 1..n,
% [
%   1,1,0,
%   0,1,0,
%   0,0,1
% ]);


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
  % "overlays_used: ", show(overlays_used), "\n",
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
