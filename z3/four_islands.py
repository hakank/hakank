#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Four Islands puzzle (Dell Logic Puzzles) in Z3
#
# http://brownbuffalo.sourceforge.net/FourIslandsClues.html
# """
# Title: Four Islands
# Author: Humphrey Dudley
# Publication: Dell Logic Puzzles
# Issue: April, 1998
# Page: 9
# Stars: 1
# 
# A tiny nation in the South Pacific contains four islands connected by bridges
# as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
# boasts a different primary export (alabaster, bananas, coconuts, and durian
# fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
# stadium, and koala preserve). Can you find the name, export, and tourist 
# attraction of each island on the map?
# 
#   N
# W   E     *compass directions
#   S
# 
# A, B, C, D are the islands
# 
# (A) -- (B)
#  |      |
#  |      |
# (C) -- (D)
# 
# 
# 1. The island noted for its koala preserve is due south of Pwana.
# 2. The island with the largest alabaster quarry is due west of Quero.
# 3. The island with the resort hotel is due east of the one that exports 
#    durian fruit.
# 4. Skern and the island with the jai alai stadium are connected by a 
#    north-south bridge. 
# 5. Rayou and the island that exports bananas are connected by an east-west
#    bridge.
# 6. The islands noted for the South Pacific's largest ice skating rink and 
#    for the jai alai stadium are not connected by a bridge.
# 
# Determine: Island island -- Island name -- Export -- Tourist Attraction
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 4

[A,B,C,D] = range(n)

island = makeIntArrayVector(sol,"island",n,0,n-1)
[Pwana, Quero, Rayou, Skern] = island

export = makeIntArrayVector(sol,"export",n,0,n-1)
[alabaster, bananas, coconuts, durian_fruit] = export

attraction = makeIntArrayVector(sol,"attraction",n,0,n-1)
[resort_hotel, ice_skating_rink, jai_alai_stadium, koala_preserve] = attraction

sol.add(Distinct(island))
sol.add(Distinct(export))
sol.add(Distinct(attraction))

# 1. The island noted for its koala preserve is due south of Pwana.
sol.add(
  Or(
    And(Pwana == A, koala_preserve == C)
    ,
    And(Pwana == B, koala_preserve == D)
  )
)  

# 2. The island with the largest alabaster quarry is due west of Quero.
sol.add( 
    Or(And(alabaster == A, Quero == B)
       ,
       And(alabaster == C, Quero == D) 
  ))

# 3. The island with the resort hotel is due east of the one that exports 
#    durian fruit.
sol.add( 
    Or(And( durian_fruit == A, resort_hotel ==  B )
       ,
      And( durian_fruit == C,resort_hotel ==  D)
  ))

# 4. Skern and the island with the jai alai stadium are connected by a 
     #    north-south bridge. 
sol.add(
     Or(And(Skern == A,jai_alai_stadium == C) 
        ,
       And(Skern == C,jai_alai_stadium == A) 
       ,
       And(Skern == B,jai_alai_stadium == D) 
       ,
       And(Skern == D, jai_alai_stadium == B) 
   ))

 # 5. Rayou and the island that exports bananas are connected by an 
     #    east-west bridge.
sol.add(
    Or(And(Rayou == A,bananas == B) 
       ,
       And(Rayou == B, bananas == A) 
       ,
       And(Rayou == C, bananas == D) 
       ,
       And(Rayou == D, bananas == C) 
  ))

# 6. The islands noted for the South Pacific's largest ice skating rink 
#    and for the jai alai stadium are not connected by a bridge.
sol.add(
    Or(
      And(ice_skating_rink == A, jai_alai_stadium == D)
      , 
      And(ice_skating_rink == D, jai_alai_stadium == A)
      ,
      And(ice_skating_rink == B, jai_alai_stadium == C)
      ,
      And(ice_skating_rink == C, jai_alai_stadium == B)
  ))


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("island    :", evalArray(mod,island))
  print("export    :", evalArray(mod,export))
  print("attraction:", evalArray(mod,attraction))
  getDifferentSolution(sol,mod,island,export,attraction)

print("num_solution:", num_solutions)
