% 
% Lights out variant puzzle in MiniZinc.
% 
% 
% Ported from 
% Erwin Kalvelagen: "A variant of the Lights Out game "
% http://yetanothermathprogrammingconsultant.blogspot.se/2016/01/a-variant-of-lights-out-game.html
% """
% The goal is to turn all lights off, but there is a catch. When flipping a light switch 
% all lamps in the same row and in the same column also flip. 
% """
%
% Also see 
% "Lights Out Variant: Flipping the whole row and column."
% http://math.stackexchange.com/questions/441571/lights-out-variant-flipping-the-whole-row-and-column/441588
% """
% ...
% Claim:
% If nxn is even, there is always a solution given any starting configuration.
%
% If nxn is odd, there is a solution iff the 'on' lights parities for each row 
% and column are the same.
%
% i.e. if the lights were 1 for on and 0 for off, then modulo 2, the sum of each individual 
% row, and sum of each individual column must be the same.
% """
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;

array[1..n,1..n] of 0..1: s0; % init

% decision variables
array[1..n,1..n] of var 0..1: s; % solution
array[1..n,1..n] of var 0..n: y; % aux

var 0..n*n: m = sum([s[i,j] | i,j in 1..n]);

% solve minimize m;
solve :: int_search([s[i,j] | i,j in 1..n] ++ [y[i,j] | i,j in 1..n], first_fail, indomain_min, complete) minimize m;

constraint
  % "final state should be even 0,2,4..."
  forall(i,j in 1..n) (
    2*y[i,j] = s0[i,j] + sum([s[i2,j] | i2 in 1..n]) + sum([s[i,j2] | j2 in 1..n]) - s[i,j]
  )
;

output [
  "m: ", show(m), "\n",
  "s:\n"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(s[i,j])

  | i,j in 1..n
]
++
["\ny:\n"]
++
[
  if j = 1 then "\n" else " " endif ++
    show(y[i,j])

  | i,j in 1..n
]
;

%
% data
%
% n = 4;
% s0 = array2d(1..n,1..n,
% [
% 1,0,0,0,
% 0,0,0,0,
% 0,1,0,0,
% 0,0,1,0,
% ]);       

%
% m = 34
%
% Solved by
% * picat_sat: 5.4s
%
n = 10;
s0 = array2d(1..n,1..n,
[
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,1,0,0,0,0,0,
0,0,0,0,0,1,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,
]);

