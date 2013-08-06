% 
% Strimko in MiniZinc.
% 
% From 
% 360: A New Twist on Latin Squares
% http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
% """
% The idea is simple: each row and column of an nxn grid must contain 
% the number 1, 2, ... n exactly once (that is, the grid must form a 
% Latin square), and each "stream" (connected path in the grid) must 
% also contain the numbers 1, 2, ..., n exactly once.
% """
%
% For more information, see:
% * http://www.strimko.com/
% * http://www.strimko.com/rules.htm
% * http://www.strimko.com/about.htm
% * http://www.puzzlersparadise.com/Strimko.htm
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n;
array[1..n, 1..n] of var 1..n: x;
array[1..n, 1..n*2] of 1..n: streams;
int: num_placed;
array[1..num_placed, 1..3] of 1..n: placed;


solve satisfy;

constraint
   %
   % latin square
   % 
   forall(i in 1..n) (
       all_different([ x[i, j] | j in 1..n]) /\
       all_different([ x[j, i] | j in 1..n])
   )
   /\
   % streams
   forall(i in 1..n) (
      all_different([x[streams[i,2*j+1],streams[i,2*j+2]] | j in 0..n-1])
   )
   /\ % placed
   forall(i in 1..num_placed) (
       x[placed[i,1], placed[i,2]] = placed[i,3]
   )

;

output
[ if j = 1 then "\n" else " " endif ++
     show(x[i,j])
  | i,j in 1..n
];

%
% data
%

% data is in the strimko_*.dzn files

% Strimko Set 068
% n = 4;
% streams = array2d(1..n, 1..n*2, [
%                     1,1, 2,2, 3,3, 4,4,
%                     2,1, 1,2, 1,3, 2,4,
%                     3,1, 4,2, 4,3, 3,4,
%                     4,1, 3,2, 2,3, 1,4
%                    ]);

% num_placed = 3;
% placed = array2d(1..num_placed, 1..3, [
%                              2,2,3,
%                              2,3,2,
%                              3,3,1
%                            ]);

