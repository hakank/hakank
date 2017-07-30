% 
% Clock Triplet Problem in MiniZinc.
% 
% Problem formulation
% http://www.f1compiler.com/samples/Dean%20Clark%27s%20Problem.f1.html
% """
% Dean Clark's Problem (Clock Triplets Problem)
%
% The problem was originally posed by Dean Clark and then presented
% to a larger audience by Martin Gardner. 
%
% The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
% by Timothy Rolfe. According to the article, in his August 1986 column for 
% Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
% 
%   Now for a curious little combinatorial puzzle involving the twelve
%   numbers on the face of a clock. Can you rearrange the numbers (keeping
%   them in a circle) so no triplet of adjacent numbers has a sum higher 
%   than 21? This is the smallest value that the highest sum of a triplet
%   can have.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

array[0..11] of var 1..12: x;
var 0..100: triplet_sum; % the sum of the triplets


% solve minimize triplet_sum; % checks if 21 really is the highest value
solve satisfy;


constraint
    triplet_sum <= 21
    /\
    all_different(x)
    /\
    x[0] = 12 /\      
    x[1] > x[11] /\   
    forall(i in 2..11) (
      x[i] + x[i-1] + x[i-2] <= triplet_sum
    )
    /\ % and around the corners
    x[10] + x[11] + x[0]  <= triplet_sum /\ 
    x[11] + x[0]  + x[1]  <= triplet_sum
;


output [
    "triplet_sum: ", show(triplet_sum), "\n",
    "       ", show(x[0]), "\n",
    "     ", show(x[11]), "    ", show(x[1]), "\n",
    "   ", show(x[10]), "       ", show(x[2]), "\n",
    "  ", show(x[9]), "         ", show(x[3]), "\n",
    "   ", show(x[8]), "        ",show(x[4]), "\n",
    "     ",  show(x[7]), "    ", show(x[5]), "\n",
    "       ", show(x[6]), "\n",
]
++
[ "x: " ++ show(x) ++ "\n"]
;