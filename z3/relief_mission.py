# 
# Relief mission in z3
#
# From PuzzlOR
# Relief Mission
# http://www.analytics-magazine.org/september-october-2010/122-the-puzzlor-relief-mission.html
# """
# Coordinating relief efforts after catastrophes such as civil unrest and 
# natural disasters can be a logistically complex challenge. Delivering 
# relief to people in need is the immediate focus of any disaster management plan.
# 
# The map in Figure 1 shows the locations of 20 villagers, each represented by 
# a "hut" icon. The villagers are in need of relief supplies contained in 
# the crates attached to parachutes. There are two identical relief packages 
# available. The only delivery option is by air drop. Each package can be dropped 
# on any cell.
# 
# After the crates are dropped, each villager will walk to the nearest drop 
# location to pick up relief supplies. Use a direct line between cells to calculate 
# travel distance. For example, the distance between A1 and A2 is 1km and the 
# distance between A1 to B2 is 1.41 km. Assume that each crate contains an 
# unlimited amount of relief supplies.
# 
# Figure 1: Where should the two relief packages be dropped?
# [
#   1 2 3 4 5 6 7 8 9 10
#   0,0,0,0,1,0,0,0,0,0, # A
#   0,0,0,0,1,0,0,0,1,1, # B
#   1,0,0,0,0,1,0,1,1,1, # C
#   0,1,0,0,0,0,1,0,0,1, # D
#   0,0,0,0,0,0,0,0,1,0, # E
#   0,0,0,0,0,0,0,1,0,0, # F
#   0,1,0,0,0,0,0,0,0,0, # G
#   0,1,0,0,0,1,0,0,0,0, # H
#   0,0,0,0,0,0,0,0,0,0, # I
#   0,0,0,0,0,0,0,1,0,1, # J
# ]
# 
# Question: Which two drop locations will minimize the total distance 
# that all villagers must travel?
# """
#
# 
# total_dist: 178
# p1: [3, 2] p2: [4, 8]
# Squared distances to nearest point (marked as *P*):
#   _   _   _   _  13   _   _   _   _   _ 
#   _   _   _   _   8   _   _   _   9  10 
#   5   _   _   _   _  10   _   5   4   5 
#   _   1 *P*   _   _   _   5   _   _   2 
#   _   2   _   _   _   _   _   _ *P*   _ 
#   _   _   _   _   _   _   _   2   _   _ 
#   _  10   _   _   _   _   _   _   _   _ 
#   _  17   _   _   _  18   _   _   _   _ 
#   _   _   _   _   _   _   _   _   _   _ 
#   _   _   _   _   _   _   _  26   _  26 
#
# There is another solution:
#  p1: [4,8] p2: [3,2]
# and is removed by the symmetry breaking constraint.
#

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
import time
from z3_utils_hakank import *


# 
# Calculate the (squared) distance between two cells.
# 
def dist(i1, j1, i2, j2):
  return Abs(i1-i2)*Abs(i1-i2) + Abs(j1-j2)*Abs(j1-j2)
  # return Abs(i1-i2)**2 + Abs(j1-j2)**2 # QF_FD don't like this!

def Min2(x,y):
  return If(x <= y, x, y)

def relief_mission(huts):

  # s = SimpleSolver()
  # s = Solver()
  # s = SolverFor("QF_LIA")
  # s = SolverFor("NIA")  
  s = SolverFor("QF_FD") # 1.992s
  # s = Optimize() # too slow

  n = len(huts)

  # The two places to drop the packages
  p1,p2 = [ [Int(f"x[{i},{j}]") for j in range(2)] for i in range(2)]
  for j in range(2):
    s.add(p1[j] >= 0, p1[j] < n,
          p2[j] >= 0, p2[j] < n)

  # all the distances (squared) from each cell to 
  # the nearest package
  distances = [ [Int(f"distances[{i},{j}") for j in range(n)] for i in range(n)]
  for i in range(n):
    for j in range(n):
      s.add(distances[i][j] >= 0, distances[i][j] <= n*n)

  total_dist = Int("total_dist")
  s.add(total_dist >= 0, total_dist <= n*n*n)
  s.add(total_dist == Sum([distances[i][j] for i in range(n) for j in range(n)]))

  #
  # For each hut, calculate the distance to the two points and pick the nearest.
  #
  for i in range(n):
    for j in range(n):
      if huts[i][j] == 1:
        dist1 = Int(f"dist1[{i},{j}]")
        dist2 = Int(f"dist2[{i},{j}]")
        # We need the domains to get QF_FD to work
        s.add(dist1 >= 0, dist1 <= n*n)
        s.add(dist2 >= 0, dist2 <= n*n)        
        
        s.add(dist1 == dist(p1[0],p1[1],i,j))
        s.add(dist2 == dist(p2[0],p2[1],i,j))
        s.add(distances[i][j] == Min2(dist1,dist2))
      
  # assign 0 distance to cells with no hut
  for i in range(n):
    for j in range(n):
      if huts[i][j] == 0:
        s.add(distances[i][j] == 0)


  # Symmetry breaking
  s.add(p1[0] < p2[0])

  # s.minimize(total_dist)

  num_solutions = 0
  while s.check() == sat:
    num_solutions += 1
    mod = s.model()
    print("total_dist:", mod[total_dist])
    p1_val = [mod.eval(p1[j]) for j in range(2)]
    p2_val = [mod.eval(p2[j]) for j in range(2)]    
    print("p1:", p1_val, "p2:", p2_val)
    print("Squared distances to nearest point (marked as *P*):")
    for i in range(n):
      for j in range(n):
        if (p1_val[0] == i and p1_val[1] == j) or (p2_val[0] == i and p2_val[1] == j):
          print("*P*", end=" ")
        elif huts[i][j] == 0:
          print("  _", end=" ")
        else:
          print(f"{mod[distances[i][j]].as_long():-3}", end=" ")
      print()
    print()
    s.add(total_dist < mod[total_dist])

  print("num_solutions:", num_solutions)


#        1 2 3 4 5 6 7 8 9 10
huts = [[0,0,0,0,1,0,0,0,0,0], # A
        [0,0,0,0,1,0,0,0,1,1], # B
        [1,0,0,0,0,1,0,1,1,1], # C
        [0,1,0,0,0,0,1,0,0,1], # D
        [0,1,0,0,0,0,0,0,1,0], # E
        [0,0,0,0,0,0,0,1,0,0], # F
        [0,1,0,0,0,0,0,0,0,0], # G
        [0,1,0,0,0,1,0,0,0,0], # H
        [0,0,0,0,0,0,0,0,0,0], # I
        [0,0,0,0,0,0,0,1,0,1]] # J

relief_mission(huts)
