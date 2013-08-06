% 
% Game Theory (2 player zero zum game) in MiniZinc.
%
% From Taha, Operations Research (8'th edition), page 528. 
% 
% Compare with the Comet model:
%   http://www.hakank.org/comet/game_theory_taha.co
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: rows;
int: cols;

array[1..rows, 1..cols] of float: game;
array[1..rows] of var float: x;
var -2.0..2.0: v;

solve maximize v;
% solve :: float_search(x, 0.00001, "input_order", "indomain_split", "complete") maximize v;


constraint
  % generalized version
  % For row player:
  forall(i in 1..rows) (
    v - sum(j in 1..cols) (x[j]* game[j,i]) <= 0.0
  )
  /\
  forall(i in 1..rows) (x[i] >= 0.0)
  /\
  sum(x) = 1.0
 
  % "talkative" version
  %  v - 3.0*x[1] + 2.0*x[2] + 5.0*x[3] <= 0.0 /\
  %  v +     x[1] - 4.0*x[2] + 6.0*x[3] <= 0.0 /\
  %  v + 3.0*x[1] +     x[2] - 2.0*x[3] <= 0.0   
  %  /\
  %  sum(x) = 1.0
  %  /\
  %  forall(i in 1..rows) (x[i] >= 0.0)
;

%
% data
%
rows = 3;
cols = 3;
game = array2d(1..rows, 1..cols, 
[ 3.0, -1.0, -3.0,
 -2.0,  4.0, -1.0, 
 -5.0, -6.0,  2.0]
);
                           

output [
"x: " ++ show(x) ++ "\n"++
"v: " ++ show(v) ++ "\n"
];
