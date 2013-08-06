% 
% Four Islands puzzle (Dell Logic Puzzles) in MiniZinc.
%
% http://brownbuffalo.sourceforge.net/FourIslandsClues.html
% """
% Title: Four Islands
% Author: Humphrey Dudley
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 9
% Stars: 1
% 
% A tiny nation in the South Pacific contains four islands connected by bridges
% as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
% boasts a different primary export (alabaster, bananas, coconuts, and durian
% fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
% stadium, and koala preserve). Can you find the name, export, and tourist 
% attraction of each island on the map?
% 
%   N
% W   E     *compass directions
%   S
% 
% A, B, C, D are the islands
% 
% (A) -- (B)
%  |      |
%  |      |
% (C) -- (D)
% 
% 
% 1. The island noted for its koala preserve is due south of Pwana.
% 2. The island with the largest alabaster quarry is due west of Quero.
% 3. The island with the resort hotel is due east of the one that exports 
%    durian fruit.
% 4. Skern and the island with the jai alai stadium are connected by a 
%    north-south bridge. 
% 5. Rayou and the island that exports bananas are connected by an east-west
%    bridge.
% 6. The islands noted for the South Pacific's largest ice skating rink and 
%    for the jai alai stadium are not connected by a bridge.
% 
% Determine: Island island -- Island name -- Export -- Tourist Attraction
% """
%
% Compare with the F1 model
% http://www.f1compiler.com/samples/Four%20Islands.f1.html
%
% Also compare with my Comet model:
% http://www.hakank.org/comet/four_islands.co
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Solution:
% A                 B               C              D
% Pwana             Skern           Rayou          Quero
% durian_fruit      coconuts        alabaster      bananas
% ice_skating_rink  resort_hotel    koala_preserve jai_alai_stadium
% 
% island: [1, 4, 3, 2]
% export: [3, 4, 2, 1]
% attraction: [2, 1, 4, 3]
%


include "globals.mzn"; 
int: n = 4;
set of int: r = 1..n;

r: A = 1;
r: B = 2;
r: C = 3;
r: D = 4;

var r: Pwana;
var r: Quero;
var r: Rayou;
var r: Skern;
array[r] of var r: island = [Pwana, Quero, Rayou, Skern];

var r: alabaster;
var r: bananas;
var r: coconuts;
var r: durian_fruit;
array[r] of var r: export = [alabaster, bananas, coconuts, durian_fruit];

var r: resort_hotel;
var r: ice_skating_rink;
var r: jai_alai_stadium;
var r: koala_preserve;
array[r] of var r: attraction = [resort_hotel, ice_skating_rink, jai_alai_stadium, koala_preserve];


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  all_different(island)
  /\
  all_different(export) 
  /\
  all_different(attraction)

  /\ % 1. The island noted for its koala preserve is due south of Pwana.
  (
    (Pwana = A /\ koala_preserve = C)
    \/
    (Pwana = B /\ koala_preserve = D)
  ) 

  /\ % 2. The island with the largest alabaster quarry is due west of Quero.
  ( 
    (alabaster = A /\ Quero = B) 
    \/ 
    (alabaster = C /\ Quero = D) 
  )

  /\ % 3. The island with the resort hotel is due east of the one that exports 
     %    durian fruit.
  ( 
    ( durian_fruit = A /\ resort_hotel =  B )
    \/
    ( durian_fruit = C /\ resort_hotel =  D)
  )

  /\ % 4. Skern and the island with the jai alai stadium are connected by a 
     %    north-south bridge. 
   (
     (Skern = A /\ jai_alai_stadium = C) 
     \/
     (Skern = C /\ jai_alai_stadium = A) 
     \/
     (Skern = B /\ jai_alai_stadium = D) 
     \/
     (Skern = D /\ jai_alai_stadium = B) 
   ) 

  /\ % 5. Rayou and the island that exports bananas are connected by an 
     %    east-west bridge.
  (
    (Rayou = A /\ bananas = B) 
    \/
    (Rayou = B /\ bananas = A) 
    \/
    (Rayou = C /\ bananas = D) 
    \/
    (Rayou = D /\ bananas = C) 
  )

  /\ % 6. The islands noted for the South Pacific's largest ice skating rink 
     %    and for the jai alai stadium are not connected by a bridge.
  ( 
   (ice_skating_rink = A /\ jai_alai_stadium = D)
   \/
   (ice_skating_rink = D /\ jai_alai_stadium = A)

   \/
   (ice_skating_rink = B /\ jai_alai_stadium = C)
   \/
   (ice_skating_rink = C /\ jai_alai_stadium = B)

  )

;

output [
 "island: ", show(island),"\n",
 "export: ", show(export),"\n",
 "attraction: ", show(attraction),"\n",

];