% 
% Nadel's construction problem in MiniZinc.
% 
% From Rina Dechter "Constraint Processing", page 5.
% Attributes the problem to
% B.A. Nadel "Constraint satisfaction algorithms" (1989).
% """
% * The recreation area should be near the lake.
% 
% * Steep slopes are to be avoided for all but the recreation area.
% * Poor soil should be avoided for those developments that 
%   involve construction, namely the apartments and the family houses.
% 
% * The highway, being noisy, should not be near the apartments, 
%   the housing, or the recreation area.
% 
% * The dumpsite should not be visible from the apartments, 
%   the houses, or the lake.
% 
% * Lots 3 and 4 have bad soil.
% * Lots 3, 4, 7, and 8 are on steep slopes .
% * Lots 2, 3, and 4 are near the lake.
% * Lots 1 and 2 are near the highway.
% """

% Comment: 
% I have not found any model that satisfies all the constraints.
% However this "soft" version counts the broken constraints
% and minimizes to 1 broken constraint.
% 
% The model (which - of course - could be erroneous) generates 28 different 
% solutions. The broken constraints are either
%   - steep_slopes constraints or
%   - near_dump constraints.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 8; % number of lots
int: d = 5; % number of developments

% matrix with the proximity of lots
array[1..n, 1..n] of 0..1: near_lots;

% the development to place in one of the lots
var 1..n: recreation;
var 1..n: apartments;
var 1..n: houses;
var 1..n: cemetery;
var 1..n: dump;
array[1..d] of var 1..n: developments = [recreation, apartments, houses, cemetery, dump];

array[1..n] of 0..1: bad_soil;
array[1..n] of 0..1: steep_slopes;
array[1..n] of 0..1: near_lake;
array[1..n] of 0..1: near_highway;

int: c = 13; % number of constraints
array[1..c] of var 0..1: broken; % indicator of broken constraint
var 0..c: total_count =  sum(broken);


% solve satisfy;
solve :: int_search(developments, first_fail, indomain_min, complete)  satisfy;
% solve minimize total_count;


constraint
  total_count <= 1 % for solve satisfy
  /\

  all_different(developments)

  /\ % * The recreation area should be near the lake.
       (near_lake[recreation] = 1 <-> broken[1] = 0)

  /\ % * Steep slopes are to be avoided for all but the recreation area.
       (steep_slopes[apartments] = 0 <-> broken[2] = 0) /\
       (steep_slopes[houses]     = 0 <-> broken[3] = 0) /\
       (steep_slopes[cemetery]   = 0 <-> broken[4] = 0) /\
       (steep_slopes[dump]       = 0 <-> broken[5] = 0)


   /\ % * Poor soil should be avoided for those developments that 
      %   involve construction, namely the apartments and the family houses.
         (bad_soil[apartments] = 0 <-> broken[6] = 0 )  /\
         (bad_soil[houses]     = 0 <-> broken[7] = 0 )

   /\ % * The highway, being noisy, should not be near the apartments, 
      %   the housing, or the recreation area.
         (near_highway[apartments] = 0 <-> broken[8] = 0)/\
         (near_highway[houses]     = 0 <-> broken[9] = 0)/\
         (near_highway[recreation] = 0 <-> broken[10] = 0)

   /\ % * The dumpsite should not be visible from the apartments, 
      %   the houses, or the lake.

         % not near the lake
         (near_lake[dump] = 0 <-> broken[11] = 0)

         /\ % not near the house 
         (
           (near_lots[dump, houses] = 0 /\ near_lots[houses, dump] = 0)
            <-> broken[12] = 0
         ) 

         /\ % not near the apartments  
         (
          (near_lots[dump, apartments] = 0 /\ near_lots[apartments, dump] = 0)
           <-> broken[13] = 0
         )
;

output [
  "[recreation, apartments, houses, cemetery, dump]\n",
  "developments (the lots) : ", show(developments), "\n",
  "la: near lake, st: steep slopes, so: bad soil, hi: near highway, du: near dump\n",
  "broken constraints: [la,st,st,st,st,so,so,hi,hi,hi,du,du,du]\n",
  "broken constraints: ", show(broken), "\n",
  "total broken constraints: ", show(total_count), "\n",  
];

% 
% data
%

% * Lots 3 and 4 have bad soil.
% * Lots 3, 4, 7, and 8 are on steep slopes .
% * Lots 2, 3, and 4 are near the lake.
% * Lots 1 and 2 are near the highway.

               % 1, 2, 3, 4, 5, 6, 7, 8
bad_soil     =  [0, 0, 1, 1, 0, 0, 0, 0];
steep_slopes =  [0, 0, 1, 1, 0, 0, 1, 1];
near_lake    =  [0, 1, 1, 1, 0, 0, 0, 0];
near_highway =  [1, 1, 0, 0, 0, 0, 0, 0];

% neighborhood matrix (for the dump placement)
near_lots = array2d(1..n, 1..n, 
[ 
% 1  2  3  4  5  6  7  8  
  0, 1, 0, 0, 1, 0, 0, 0, % 1
  1, 0, 1, 0, 0, 1, 0, 0, % 2 
  0, 1, 0, 1, 0, 0, 1, 0, % 3 
  0, 0, 1, 0, 0, 0, 0, 1, % 4
  1, 0, 0, 0, 0, 1, 0, 0, % 5
  0, 1, 0, 0, 1, 0, 1, 0, % 6
  0, 0, 1, 0, 0, 1, 0, 1, % 7
  0, 0, 0, 1, 0, 0, 1, 0, % 8

]);


% alternative neighborhood matrix,
% where diagonals also makes a neighbour.
% This generates 8 models (all with 1 broken constraint)
%
% near_lots = array2d(1..n, 1..n, 
% [ 
% % 1  2  3  4  5  6  7  8  
%   0, 1, 0, 0, 1, 1, 0, 0, % 1
%   1, 0, 1, 0, 1, 1, 1, 0, % 2 
%   0, 1, 0, 1, 0, 1, 1, 1, % 3 
%   0, 0, 1, 0, 0, 0, 1, 1, % 4
%   1, 1, 0, 0, 0, 1, 0, 0, % 5
%   1, 1, 1, 0, 1, 0, 1, 0, % 6
%   0, 1, 1, 1, 0, 1, 0, 1, % 7
%   0, 0, 1, 1, 0, 0, 1, 0, % 8

% ]);
