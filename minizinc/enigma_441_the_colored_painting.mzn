% 
% Enigma 441: The coloured painting in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2018/03/23/enigma-441-the-coloured-painting/
% """
% From New Scientist #1591, 17th December 1987 [link]
%
%  I looked down at the body slumped over my desk. One hand held my card 
% “Newton Harlowe — Private detective”, and the other a painting. All I knew about 
% painting came from watching my secretary Velda doing her nails. However, I could see 
% in the dim light that is was a 6 × 6 array of small squares, each coloured red or 
% blue or green. As the neon lights on the nightclubs opposite my office window flashed 
% on and off and the light reflected from the wet sidewalks, I was able to make out the 
% vertical columns of the painting. I saw:
%
%     [
%      RBGGBR
%      BBGRRG
%      GGRBRB
%      RGBRBG
%      GRRBGB 
%      BRBGGR
%    ]
 

% though that was not necessarily the order they occurred in the painting. Suddenly the 
% door opened and a raincoated figure with an automatic entered. There was a loud bang 
% and everything went black.
%
% I came round to find myself lying next to the body of a blonde on the floor of a living 
% room. From the sound of the surf outside I could tell it was a beach-house. There on the 
% wall was the painting. The moonlight shone onto it through the shutters. As they moved 
% in the breeze I was able to make out the horizontal rows of the painting. I saw:
%
%     [ 
%      BGBRGR
%      RBBGRG
%      GGRBRB
%      RRGBGB
%      GBRRBG
%      BRGGBR
%     ]
%
% though again not necessarily in the right order. Just then a police siren sounded 
% outside. I was going to have to do some explaining, and that painting was the key.
%
% Reproduce the painting.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;
enum colors = {R,G,B};

% rows, in some order
array[1..n, 1..n] of colors: rows = 
  array2d(1..n,1..n,
[
     B,G,B,R,G,R,
     R,B,B,G,R,G,
     G,G,R,B,R,B,
     R,R,G,B,G,B,
     G,B,R,R,B,G,
     B,R,G,G,B,R,
]);

% columns, in some order
array[1..n, 1..n] of colors: cols = 
  array2d(1..n,1..n,
[
     R,B,G,G,B,R,
     B,B,G,R,R,G,
     G,G,R,B,R,B,
     R,G,B,R,B,G,
     G,R,R,B,G,B, 
     B,R,B,G,G,R,
]);


% decision variables
array[1..n] of var 1..n: rows_order;
array[1..n] of var 1..n: cols_order;

solve satisfy;
% solve :: int_search(rows_order ++ cols_order, first_fail, indomain_split, complete) satisfy;

constraint all_different(rows_order);
constraint all_different(cols_order);
constraint
   forall(i,j in 1..n) (
      rows[rows_order[i],j] = cols[cols_order[j],i]
   ) 
;

output [
  "rows_order: \(rows_order)\n",
  "cols_order: \(cols_order)\n",
]
++
[
  "\([rows[i,j] | j in 1..n])\n"
  | i in fix(rows_order)
];



