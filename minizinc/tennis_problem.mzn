% 
% Tennis problem in MiniZinc.
% 
% From Jean-Louis Laurière "Problem Solving and Artificial Intelligence",
% page 422pp:
% """
% Frank and George play tennis.
% Frank beats George 6 game to 3
% In 4 games the server loses.
% Who served the first game?
% """

% There are 40 solutions to this problem. 
% For all solutions George serves first, i.e. gs[1] = 1.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 9; % 6 + 3 games


% decision variables
array[1..n] of var 0..1: fs; % 1 if Frank serves
array[1..n] of var 0..1: gs; % 1 if George serves
array[1..n] of var 0..1: fw; % 1 if Frank wins game
array[1..n] of var 0..1: gw; % 1 if George wins game


solve satisfy;
% solve :: int_search(gs ++ fs, first_fail, indomain_min, complete) satisfy;

constraint
  forall(j in 1..n) (
    fs[j] = 1-gs[j] /\
    fw[j] = 1-gw[j]
  )
  /\
  forall(j in 2..n) (
     gs[j] = 1-gs[j-1]
  )
  /\ % frank wins 6 games
  sum(fw) = 6 

  /\ % in 4 games the serves loses 
  sum([fs[j]*gw[j] + gs[j]*fw[j] | j in 1..n]) = 4

  % /\ gs[1] != 1 % checking

;

output 
[
  "first_serves [g,f]: ", show([gs[1],fs[1]]), "\n",
  "fs: ", show(fs), "\n",
  "gs: ", show(gs), "\n",
  "fw: ", show(fw), "\n",
  "gw: ", show(gw), "\n",
  
];

