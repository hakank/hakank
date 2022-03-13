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
# See nadel.py for the original version using soft constraints to get the
# minimimal number of broken constraints. 
#
# This program uses instead assert_and_track() to get the unsat_core.
# Here are the occurrences of the constraints that are broken in
# solutions with only one broken constraint:
# (from nadel.py)
#   1 : 8
#   2 : 8
#   4 : 8
#  11 : 2
#  12 : 2
#
# This is what unsat_core yields:
#  [broken[12], broken[11], broken[4], broken[2], broken[1]]
#
# Or with a explicit names:
# [near_lots_a_dump_n_apartments_and_near_lots_a_apartments_n_dump,
#  near_lots_a_dump_n_houses_and_near_lots_a_houses_n_dump,
#  steep_slopes_a_dump,
#  steep_slopes_a_houses,
#  steep_slopes_a_apartments]
#
# I.e. it's all the possible constraints that can be broken!
#
# Note: If ":core.minimize" is set to False, then we get the following
# unsat core. This also includes the non-minimal solutions that have more
# than single broken constraint:
#
# [broken[1],
# broken[2],
# broken[3],
# broken[4],
# broken[7],
# broken[8],
# broken[10],
# broken[11],
# broken[12]]
# 
# Or with the explicit names:
# [steep_slopes_a_apartments,
# steep_slopes_a_houses,
# steep_slopes_a_cemetery,
# steep_slopes_a_dump,
# near_highway_a_apartments,
# near_highway_a_houses,
# near_lake_a_dump,
# near_lots_a_dump_n_houses_and_near_lots_a_houses_n_dump,
# near_lots_a_dump_n_apartments_and_near_lots_a_apartments_n_dump]

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

# From https://theory.stanford.edu/~nikolaj/programmingz3.html
def set_core_minimize(s):
  # s.set("sat.core.minimize","true")  # For Bit-vector theories
  # s.set("smt.core.minimize","true")  # For general SMT
  # From https://stackoverflow.com/questions/56021650/generating-minimal-unsat-core-using-z3-python-api
  s.set(':core.minimize', False)

def nadel(opt_value=None):

  this_opt_value = None
  if opt_value != None:
    this_opt_value = opt_value

  # sol = Optimize() # Doesn't have :core.minimize!
  sol = SimpleSolver()
  # sol = SolverFor("NIA")

  set_core_minimize(sol)

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

  # indicators of broken constraint 
  broken = [Bool(f"broken[{i}]") for i in range(num_constraints)]
  # Give the constraints better names:
  # broken = Bools("near_lake_a_recreation steep_slopes_a_apartments steep_slopes_a_houses steep_slopes_a_cemetery steep_slopes_a_dump bad_soil_a_apartments bad_soil_a_houses near_highway_a_apartments near_highway_a_houses near_highway_a_recreation near_lake_a_dump near_lots_a_dump_n_houses_and_near_lots_a_houses_n_dump near_lots_a_dump_n_apartments_and_near_lots_a_apartments_n_dump")

  total_broken = makeIntVar(sol,"total_broken", 0, num_constraints)

  # constraints

  sol.add(total_broken == Sum([If(broken[i],1,0) for i in range(num_constraints)]))
  sol.add(Distinct(developments))

  # * The recreation area should be near the lake.
  sol.assert_and_track(near_lake_a[recreation] == 1, broken[0])

  # * Steep slopes are to be avoided for all but the recreation area.
  sol.assert_and_track(steep_slopes_a[apartments] == 0, broken[1])
  sol.assert_and_track(steep_slopes_a[houses]     == 0, broken[2])
  sol.assert_and_track(steep_slopes_a[cemetery]   == 0, broken[3])
  sol.assert_and_track(steep_slopes_a[dump]       == 0, broken[4])


  # * Poor soil should be avoided for those developments that 
  #   involve construction, namely the apartments and the family houses.
  sol.assert_and_track(bad_soil_a[apartments] == 0, broken[5])
  sol.assert_and_track(bad_soil_a[houses]     == 0, broken[6])
  
  # * The highway, being noisy, should not be near the apartments, 
  #   the housing, or the recreation area.
  sol.assert_and_track(near_highway_a[apartments] == 0, broken[7])
  sol.assert_and_track(near_highway_a[houses]     == 0, broken[8])
  sol.assert_and_track(near_highway_a[recreation] == 0, broken[9])

  # The dumpsite should not be visible from the apartments, 
  # the houses, or the lake.
  
  # not near the lake
  sol.assert_and_track(near_lake_a[dump] == 0, broken[10])

  # not near the house 
  sol.assert_and_track(
    And(near_lots_a[dump*n+houses] == 0, near_lots_a[houses*n+dump] == 0), broken[11])

  # not near the apartments  
  sol.assert_and_track(
    And(near_lots_a[dump*n+apartments] == 0, near_lots_a[apartments*n+dump] == 0), broken[12])

  # for showing all optimal solutions
  if opt_value != None:
    sol.add(total_broken == this_opt_value)

  # sol.minimize(total_broken)

  num_solutions = 0
  if sol.check() == sat:
    # Note: We don't get any solutions!
    num_solutions += 1
    mod = sol.model()
    print("broken:", [mod.eval(broken[i]) for i in range(c)])
    print("developments:", [mod.eval(d) for d in developments])
    print("total_broken :", mod.eval(total_broken))
    print()
    getDifferentSolution(sol,mod,developments)
    if opt_value == None:
      this_opt_value = mod.eval(total_broken)
      getLessSolution(sol,mod, total_broken)
  else:
    # Just an unsat core
    print("unsat_core:")
    print(sol.unsat_core())
      
nadel()
