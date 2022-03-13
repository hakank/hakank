#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Nadel's construction problem in Z3
#
# From Rina Dechter "Constraint Processing", page 5.
# Attributes the problem to
# B.A. Nadel "Constraint satisfaction algorithms" (1989).
# """
# * The recreation area should be near the lake.
# 
# * Steep slopes are to be avoided for all but the recreation area.
# * Poor soil should be avoided for those developments that 
#   involve construction, namely the apartments and the family houses.
# 
# * The highway, being noisy, should not be near the apartments, 
#   the housing, or the recreation area.
# 
# * The dumpsite should not be visible from the apartments, 
#   the houses, or the lake.
# 
# * Lots 3 and 4 have bad soil.
# * Lots 3, 4, 7, and 8 are on steep slopes .
# * Lots 2, 3, and 4 are near the lake.
# * Lots 1 and 2 are near the highway.
# """
#
# Comment: 
# I have not found any model that satisfies all the constraints.
# However this "soft" version counts the broken constraints
# and minimizes to 1 broken constraint.
# 
# The model (which - of course - could be erroneous) generates 28 different 
# solutions. The broken constraints are either
#   - steep_slopes constraints or
#   - near_dump constraints.
#
# Here are the occurrences of the constraints that are broken in
# solutions with only one broken constraint:
#   1 : 8
#   2 : 8
#   4 : 8
#  11 : 2
#  12 : 2
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from collections import defaultdict
from z3_utils_hakank import *


def nadel(opt_value=None):

  this_opt_value = None
  if opt_value != None:
    this_opt_value = opt_value

  # sol = Optimize()
  sol = Solver()
  # sol = SolverFor("NIA")

  n = 8 # number of lots
  d = 5 # number of developments


  # * Lots 3 and 4 have bad soil.
  # * Lots 3, 4, 7, and 8 are on steep slopes .
  # * Lots 2, 3, and 4 are near the lake.
  # * Lots 1 and 2 are near the highway.

                 # 1, 2, 3, 4, 5, 6, 7, 8
  bad_soil     =  [0, 0, 1, 1, 0, 0, 0, 0]
  steep_slopes =  [0, 0, 1, 1, 0, 0, 1, 1]
  near_lake    =  [0, 1, 1, 1, 0, 0, 0, 0]
  near_highway =  [1, 1, 0, 0, 0, 0, 0, 0]

  # as Arrays to make them "elementable"
  bad_soil_a     = copyArray(sol,bad_soil,"bad_soil",0,1)
  steep_slopes_a = copyArray(sol,steep_slopes,"steep_slopes",0,1)
  near_lake_a    = copyArray(sol,near_lake,"near_lake",0,1)
  near_highway_a = copyArray(sol,near_highway,"near_highway",0,1)

  # matrix with the proximity of lots
  # (for the dump placement)
  near_lots = [ 
    # 1  2  3  4  5  6  7  8  
    [0, 1, 0, 0, 1, 0, 0, 0], # 1
    [1, 0, 1, 0, 0, 1, 0, 0], # 2 
    [0, 1, 0, 1, 0, 0, 1, 0], # 3 
    [0, 0, 1, 0, 0, 0, 0, 1], # 4
    [1, 0, 0, 0, 0, 1, 0, 0], # 5
    [0, 1, 0, 0, 1, 0, 1, 0], # 6
    [0, 0, 1, 0, 0, 1, 0, 1], # 7
    [0, 0, 0, 1, 0, 0, 1, 0]  # 8
    ]


  # alternative neighborhood matrix,
  # where diagonals also makes a neighbour.
  # This generates 8 models (all with 1 broken constraint)
  #
  # near_lots = [ 
  # # 1  2  3  4  5  6  7  8  
  #  [0, 1, 0, 0, 1, 1, 0, 0], # 1
  #  [1, 0, 1, 0, 1, 1, 1, 0], # 2 
  #  [0, 1, 0, 1, 0, 1, 1, 1], # 3 
  #  [0, 0, 1, 0, 0, 0, 1, 1], # 4
  #  [1, 1, 0, 0, 0, 1, 0, 0], # 5
  #  [1, 1, 1, 0, 1, 0, 1, 0], # 6
  #  [0, 1, 1, 1, 0, 1, 0, 1], # 7
  #  [0, 0, 1, 1, 0, 0, 1, 0]  # 8
  # ]

  # Array copy of near_lots (for element constraint)
  near_lots_a = makeIntArray(sol,"near_lots",n*n, 0,1)
  for i in range(n):
    for j in range(n):
      sol.add(near_lots_a[i*n+j] == near_lots[i][j])
      
  #
  # variables
  #


  # the development to place in one of the lots
  recreation = makeIntVar(sol,"recreation", 0,n-1)
  apartments = makeIntVar(sol,"apartments", 0,n-1)
  houses     = makeIntVar(sol,"houses", 0,n-1)
  cemetery   = makeIntVar(sol,"cemetery", 0,n-1)
  dump       = makeIntVar(sol,"dump", 0,n-1)
  developments = [recreation, apartments, houses, cemetery, dump]

  
  num_constraints = 13 # number of (potentially) broken constraints (soft constraints)
  
  broken = makeIntVector(sol,"broken", num_constraints, 0,1) # indicator of broken constraint 
  total_broken = makeIntVar(sol,"total_broken", 0, num_constraints) #  sum(broken)

  # constraints

  sol.add(total_broken == Sum(broken))
  sol.add(Distinct(developments))

  # * The recreation area should be near the lake.
  sol.add((near_lake_a[recreation] == 1) == ( broken[0] == 0))

  # * Steep slopes are to be avoided for all but the recreation area.
  sol.add((steep_slopes_a[apartments] == 0) == (broken[1] == 0))
  sol.add((steep_slopes_a[houses]     == 0) == (broken[2] == 0))
  sol.add((steep_slopes_a[cemetery]   == 0) == (broken[3] == 0))
  sol.add((steep_slopes_a[dump]       == 0) == (broken[4] == 0))


  # * Poor soil should be avoided for those developments that 
  #   involve construction, namely the apartments and the family houses.
  sol.add((bad_soil_a[apartments] == 0) == (broken[5] == 0))
  sol.add((bad_soil_a[houses]     == 0) == (broken[6] == 0))
  
  # * The highway, being noisy, should not be near the apartments, 
  #   the housing, or the recreation area.
  sol.add((near_highway_a[apartments] == 0) == (broken[7] == 0))
  sol.add((near_highway_a[houses]     == 0) == (broken[8] == 0))
  sol.add((near_highway_a[recreation] == 0) == (broken[9] == 0))

  # The dumpsite should not be visible from the apartments, 
  # the houses, or the lake.
  
  # not near the lake
  sol.add((near_lake_a[dump] == 0) == (broken[10] == 0))

  # not near the house 
  sol.add(
    And(near_lots_a[dump*n+houses] == 0, near_lots_a[houses*n+dump] == 0) == (broken[11] == 0)
    ) 

  # not near the apartments  
  sol.add(
    And(near_lots_a[dump*n+apartments] == 0, near_lots_a[apartments*n+dump] == 0) == (broken[12] == 0)
    )

  # for showing all optimal solutions
  if opt_value != None:
    sol.add(total_broken == this_opt_value)

  # sol.minimize(total_broken)

  num_solutions = 0
  broken_hash = {}
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    broken_val = [mod.eval(broken[i]).as_long() for i in range(num_constraints)]
    print("broken:", broken_val)
    developments_val = [mod.eval(d) for d in developments]
    print("developments:", developments_val)
    total_broken_val = mod.eval(total_broken)
    print("total_broken :", total_broken_val)
    print()
    for c in range(num_constraints):
      if broken_val[c] == 1:
        broken_hash[c] = broken_hash.get(c,0) + 1
        
    getDifferentSolution(sol,mod,developments)
    if opt_value == None:
      this_opt_value = total_broken_val
      getLessSolution(sol,mod, total_broken)
      
  print("num_solutions:", num_solutions)
  print("occurrences of broken constraints:")
  for c in sorted(broken_hash.keys()):
    print(c,":", broken_hash[c])
    
  return this_opt_value

opt_val = nadel(None)
print("Optimal value is", opt_val)
print("Now show all possible optimal values:")
nadel(opt_val)
