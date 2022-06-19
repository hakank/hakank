"""
Four Islands puzzle (Dell Logic Puzzles) in cpmpy.

http://brownbuffalo.sourceforge.net/FourIslandsClues.html
'''
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
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def four_islands():

  model = Model()

  n = 4

  [A,B,C,D] = range(n)

  island = intvar(0,n-1,shape=n,name="island")
  Pwana, Quero, Rayou, Skern = island

  export = intvar(0,n-1,shape=n,name="export")
  alabaster, bananas, coconuts, durian_fruit = export

  attraction = intvar(0,n-1,shape=n,name="attraction")
  resort_hotel, ice_skating_rink, jai_alai_stadium, koala_preserve = attraction

  model += [AllDifferent(island),
            AllDifferent(export),
            AllDifferent(attraction)]
  
  # 1. The island noted for its koala preserve is due south of Pwana.
  model += [    
    ( (Pwana == A) & (koala_preserve == C))
    |
    ( (Pwana == B) & (koala_preserve == D))
    ]

  # 2. The island with the largest alabaster quarry is due west of Quero.
  model += [ 
       ( (alabaster == A) & (Quero == B))
       |
       ( (alabaster == C) & (Quero == D))
       ]
  
  # 3. The island with the resort hotel is due east of the one that exports 
  #    durian fruit.
  model += [ 
    ( (durian_fruit == A) & (resort_hotel ==  B ))
     |
     ( (durian_fruit == C) & (resort_hotel ==  D))
     ]
  
  # 4. Skern and the island with the jai alai stadium are connected by a 
  #    north-south bridge. 
  model += [
       ((Skern == A) & (jai_alai_stadium == C) )
       |
       ((Skern == C) & (jai_alai_stadium == A) )
       |
       ((Skern == B) & (jai_alai_stadium == D) )
       |
       ((Skern == D) & (jai_alai_stadium == B) )
       ]
  
  # 5. Rayou and the island that exports bananas are connected by an 
  #    east-west bridge.
  model += [
       ((Rayou == A) & (bananas == B) )
       |
       ((Rayou == B) & (bananas == A) )
       |
       ((Rayou == C) & (bananas == D) )
       |
       ((Rayou == D) & (bananas == C) )
       ]
  
  # 6. The islands noted for the South Pacific's largest ice skating rink 
  #    and for the jai alai stadium are not connected by a bridge.
  model += [
    ((ice_skating_rink == A) & (jai_alai_stadium == D))
    |
    ((ice_skating_rink == D) & (jai_alai_stadium == A))
    |
    ((ice_skating_rink == B) & (jai_alai_stadium == C))
    |
    ((ice_skating_rink == C) & (jai_alai_stadium == B))
    ]

  def print_sol():
    print("island:",island.value())
    print("export:",export.value())
    print("attraction:",attraction.value())
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


four_islands()
