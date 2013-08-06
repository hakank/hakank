/*

  Four Islands puzzle (Dell Logic Puzzles) in AMPL+CP.

  http://brownbuffalo.sourceforge.net/FourIslandsClues.html
  """
  Title: Four Islands
  Author: Humphrey Dudley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  
  A tiny nation in the South Pacific contains four islands connected by bridges
  as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
  boasts a different primary export (alabaster, bananas, coconuts, and durian
  fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
  stadium, and koala preserve). Can you find the name, export, and tourist 
  attraction of each island on the map?
  
    N
  W   E     *compass directions
    S
  
  A, B, C, D are the islands
  
  (A) -- (B)
   |      |
   |      |
  (C) -- (D)
  
  
  1. The island noted for its koala preserve is due south of Pwana.
  2. The island with the largest alabaster quarry is due west of Quero.
  3. The island with the resort hotel is due east of the one that exports 
     durian fruit.
  4. Skern and the island with the jai alai stadium are connected by a 
     north-south bridge. 
  5. Rayou and the island that exports bananas are connected by an east-west
     bridge.
  6. The islands noted for the South Pacific's largest ice skating rink and 
     for the jai alai stadium are not connected by a bridge.
  
  Determine: Island island -- Island name -- Export -- Tourist Attraction
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n := 4;
param A := 1;
param B := 2;
param C := 3;
param D := 4;

set Islands;
set Exports;
set Attractions;

# decision variables
var island{Islands} >= 1 <= n integer;
var export{Exports} >= 1 <= n integer;
var attraction{Attractions} >= 1 <= n integer;

#
# constraints
#
s.t. c0a: alldiff{i in Islands} island[i];
s.t. c0b: alldiff{i in Exports} export[i];
s.t. c0c: alldiff{i in Attractions} attraction[i];

# 1. The island noted for its koala preserve is due south of Pwana.
s.t. c1: 
   (island['Pwana'] = A and attraction['koala_preserve'] = C)
   or
   (island['Pwana'] = B and attraction['koala_preserve'] = D)

;

# 2. The island with the largest alabaster quarry is due west of Quero.
s.t. c2: 
  (export['alabaster'] = A and island['Quero'] = B) 
  or 
  (export['alabaster'] = C and island['Quero'] = D) 
;

# 3. The island with the resort hotel is due east of the one that exports 
#    durian fruit.
s.t. c3:
  ( export['durian_fruit'] = A and attraction['resort_hotel'] =  B )
  or
  ( export['durian_fruit'] = C and attraction['resort_hotel'] =  D)
;

# 4. Skern and the island with the jai alai stadium are connected by a 
#    north-south bridge. 
s.t. c4:
   (island['Skern'] = A and attraction['jai_alai_stadium'] = C) 
   or
   (island['Skern'] = C and attraction['jai_alai_stadium'] = A) 
   or
   (island['Skern'] = B and attraction['jai_alai_stadium'] = D) 
   or
   (island['Skern'] = D and attraction['jai_alai_stadium'] = B) 
;

# 5. Rayou and the island that exports bananas are connected by an 
#    east-west bridge.
s.t. c5:
  (island['Rayou'] = A and export['bananas'] = B) 
  or
  (island['Rayou'] = B and export['bananas'] = A) 
  or
  (island['Rayou'] = C and export['bananas'] = D) 
  or
  (island['Rayou'] = D and export['bananas'] = C) 
;

# 6. The islands noted for the South Pacific's largest ice skating rink 
#    and for the jai alai stadium are not connected by a bridge.
s.t. c6:
 (attraction['ice_skating_rink'] = A and attraction['jai_alai_stadium'] = D)
 or
 (attraction['ice_skating_rink'] = D and attraction['jai_alai_stadium'] = A)
 or
 (attraction['ice_skating_rink'] = B and attraction['jai_alai_stadium'] = C)
 or
 (attraction['ice_skating_rink'] = C and attraction['jai_alai_stadium'] = B)
;


data;

set Islands = Pwana Quero Rayou Skern;
set Exports = alabaster bananas coconuts durian_fruit;
set Attractions = resort_hotel ice_skating_rink jai_alai_stadium koala_preserve;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display island, export,attraction;

for{i in 1..n} {
  printf "%d: ", i;
  for{j in Islands} { if island[j] = i then printf "%-5s ", j };
  for{j in Exports} { if export[j] = i then printf "%-14s ", j };
  for{j in Attractions} { if attraction[j] = i then { printf "%-16s ", j} };
  printf "\n";

}