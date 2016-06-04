% 
% Golf puzzle in MiniZinc.
% 
% http://blogs.sas.com/content/operations/2016/03/03/golf-puzzle/
% """
% Here's a golf puzzle from Sam Loyd:
%
% Everybody is playing golf now, and even the lazy ones who a few weeks ago declared 
% how much pleasanter it was to swing in a shady hammock, have caught the golf fever 
% and are chasing the ball around the golf links. I am not much of a golfer, but I have 
% met a genius who has a winning system based on mathematics. He says: "Just cultivate 
% two strokes of different lengths, one a drive, the other an approach, and play 
% directly toward the hole so that a combination of the two distances will get you there."
% 
% What should be the proper lengths of strokes to learn that would make possible the 
% lowest score on a nine-hole course, of 
%    150 yards, 300 yards, 250 yards, 325 yards, 275 yards, 350 yards, 
%    225 yards, 400 yards, and 425 yards? 
% The ball must go the full length on each stroke, but you may go beyond the hole 
% with either stroke, then play back toward the hole. All strokes are on a straight 
% line toward the hole.
% """

% 
% The two optimal solutions with number of strokes (z) = 26 are:
%
% stroke1: 125
% stroke2: 150
% z: 26
%
%   0   1
%   0   2
%   2   0
%  -1   3
%   1   1
%   4  -1
%   3  -1
%   2   1
%   1   2
% ----------
%
% stroke1: 75
% stroke2: 175
% z: 26
%
%   2   0
%   4   0
%   1   1
%   2   1
%  -1   2
%   0   2
%   3   0
%   3   1
%   1   2
% ----------

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 9;
array[1..n] of int: data = [150,300,250,325,275,350,225,400,425];

% decision variables
var 1..1000: club1; % = 75;
var 1..1000: club2; % = 175;

% number of swings with each club for each hole
array[1..n,1..2] of var -10..10: holes;
var int: z;

solve minimize z;
% solve satisfy;
% solve :: int_search([club1,club2] ++ array1d(holes), anti_first_fail, indomain_split, complete) minimize z;

constraint
   forall(i in 1..n) (
     data[i] = holes[i,1]*club1 + holes[i,2]*club2 
   )
   /\ 
   z = sum([abs(holes[i,j]) | i in 1..n, j in 1..2]) 

   /\ club1 <= club2 % symmetry breaking

   % /\
   % z = 26
;

output [
  "club1: \(club1)\n",
  "club2: \(club2)\n",
  "z: \(z)\n",
]
++
[
 if j = 1 then "\n" else " " endif ++
    show_int(3,holes[i,j])
  | i in 1..n, j in 1..2
]
;

