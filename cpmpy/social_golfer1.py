"""
Social golfer problem in cpmpy.

CSPLib problem 10:
http://www.csplib.org/prob/prob010/index.html
'''
The coordinator of a local golf club has come to you with the following 
problem. In her club, there are 32 social golfers, each of whom play golf 
once a week, and always in groups of 4. She would like you to come up 
with a schedule of play for these golfers, to last as many weeks as 
possible, such that no golfer plays in the same group as any other golfer 
on more than one occasion.

Possible variants of the above problem include: finding a 10-week schedule 
with ``maximum socialisation''; that is, as few repeated pairs as possible 
(this has the same solutions as the original problem if it is possible 
to have no repeated pairs), and finding a schedule of minimum length 
such that each golfer plays with every other golfer at least once 
(``full socialisation'').

The problem can easily be generalized to that of scheduling m groups of 
n golfers over p weeks, such that no golfer plays in the same group as any 
other golfer twice (i.e. maximum socialisation is achieved). 
'''


This model is a translation of the OPL code from 
http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=010

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
import math, sys, os
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

 

def social_golfer1(weeks=4,groups=3,groupSize=3,num_sols=1):

  golfers = groups * groupSize
  print("golfers:",golfers,"weeks:",weeks,"groupSize:",groupSize,"groups:",groups)

  
  Golfer = list(range(golfers))
  Week = list(range(weeks))
  Group = list(range(groups))


  # Search space: The set of all possible group assignments to all players 
  # in each of the weeks weeks.
  assign = intvar(0,groups-1,shape=(golfers,weeks),name="assign")

  model = Model()

  # C1: Each group has exactly groupSize players
  for gr in Group:
    for w in Week:
      model += [sum([(assign[g,w] == gr) for g in Golfer])  == groupSize]

  # C2: Each pair of players only meets at most once
  for g1 in Golfer:
    for g2 in Golfer:
      if g1 != g2:
        for w1 in Week:
          for w2 in Week:
            if w1 != w2:
              model += [(assign[g1,w1] == assign[g2,w1]) + (assign[g1,w2] == assign[g2,w2]) <= 1]
  

  # SBSA: Symmetry-breaking by selective assignment
  # On the first week, the first groupSize golfers play in group 1, the 
  # second groupSize golfers play in group 2, etc. On the second week, 
  # golfer 1 plays in group 1, golfer 2 plays in group 2, etc.
  for g in Golfer:
    model += [assign[g,0] == math.floor((g) / groupSize) ]

  for g in Golfer:
    if g < groupSize:
      model += [assign[g,1]==g]

  # First golfer always in group 0
  for w in Week:
    model += [assign[0,w] == 0]
  
  # print(model)

  def print_sol():
    print("assign:")
    assign_val = assign.value()
    print(assign_val)
    print()
    
    meets = {}
    for g in range(1,golfers+1):
      meets[g] = []
    print("Schedule:")
    for w in range(weeks):
      print("week:",w+1)
      for gr in range(groups):
        print("group ",gr+1, end=": ")
        gs = [g+1 for g in range(golfers) if assign_val[g,w] == gr]
        print("golfers:","".join([f"{g:3d}" for g in gs]))
        for g1 in gs:
          for g2 in gs:
            if g1 < g2:
              meets[g1].append(g2)
              meets[g2].append(g1)
    print()
    for g in range(1,golfers+1):
      print(f"Golfer {g} meets:", sorted(meets[g]))
    print(flush=True)


  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.log_search_progress = True
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
  print("num_solutions:", num_solutions)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())
  
  print()

# weeks = 4
# groups = 3
# groupSize = 2

# weeks = 5
# groups = 7
# groupSize = 3

# 24 golfers, 6 groups, 4 weeks
weeks = 4
groups = 6
groupSize = 4

# weeks = 7
# groups = 5
# groupSize = 3

num_sols = 1
social_golfer1(weeks,groups,groupSize,num_sols)
