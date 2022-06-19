"""
Water buckets problem (generalized) in cpmpy.

From http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=018
'''
Problem description
This is a generalization of the CSPLib specification, which is as follows: 
Given an 8 pint bucket of water, and two empty buckets which can contain 
5 and 3 pints respectively, the problem requires to divide the water into 
two by pouring water between buckets (that is, to end up with 4 pints in 
the 8 pint bucket, and 4 pints in the 5 pint bucket) in the smallest number 
of transfers.

The generalization consists in making the specification parametric with 
respect to the start and goal configurations, which are now inputs to the problem.

Problem input

  * Function start, assigning an initial amount of water to each bucket
  * Function goal, assigning the goal amount of water to each bucket 

Search space
The set of all possibile sequences of configurations (states), where a 
configuration is a triple encoding the amount of water in each bucket at 
a given time-step

Constraints

  * C1: At beginning, buckets contain the amount of water specified by 
        function start
  * C2: At the end, buckets contain the amount of water specified by 
        function goal
  * C3: The configuration at each step differs from that of the next one 
        in that the amount of water of exactly 2 buckets changes (that in 
        the others remain the same)
  * C4: The overall amount of water is the same at each time step
  * C5: After each transition (involving buckets b1 and b2), either the 
        source bucket becomes empty, or the target becomes full
'''

Translation from the OPL code at
http://www.dis.uniroma1.it/~tmancini/index.php?problemid=018&solver=OPL&spec=BASE&currItem=research.publications.webappendices.csplib2x.problemDetails#listing

Note: Most of the comments below are from the OPL model (cited above).

Here's a solution for the original problem:
   capacity: [8, 5, 3]
   start state: [8, 0, 0]
   goal state : [4, 4, 0]
   max_step : 10
   goal_step: 7
   state:
   [[8 0 0]
   [3 5 0]
   [3 2 3]
   [6 2 0]
   [6 0 2]
   [1 5 2]
   [1 4 3]
   [4 4 0]
   [4 1 3]
   [4 0 4]]
   change:
   [[1 1 0]
   [0 1 1]
   [1 0 1]
   [0 1 1]
   [1 1 0]
   [0 1 1]
   [1 0 1]
   [0 1 1]
   [0 1 1]]
   The solution:
   step 0: [8 0 0]
   step 1: [3 5 0]
   step 2: [3 2 3]
   step 3: [6 2 0]
   step 4: [6 0 2]
   step 5: [1 5 2]
   step 6: [1 4 3]
   step 7: [4 4 0]

Testing all 24 possible goal states for this puzzle, we find that
the following goal states don't have a solution in 30 steps:

  [4, 2, 2]
  [5, 2, 1]
  [3, 4, 1]
  [5, 1, 2]
  [2, 4, 2]
  [6, 1, 1]
  [4, 3, 1]
  [3, 3, 2]



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def water_buckets1(start,goal,capacity,max_step=10):

  print("capacity:", capacity)
  print("start state:", start)
  print("goal state :", goal) 
  print("max_step :", max_step) 

  nb_buckets = len(start)
  buckets = range(nb_buckets)
  
  steps = range(max_step)
  max_capacity = max(capacity)

  # Checking the problem
  assert sum(start) == sum(goal), "Sum of start must be == sum of goal "
  for b in buckets:
    assert start[b] <= capacity[b], f"bucket {b}: start[b] must be <= capacity[b]"
    assert goal[b] <= capacity[b], f"bucket {b}: goal[b] must be <= capacity[b]"    

  # Varibles
  # Search space: The set of all possibile sequences of configurations 
  # (states), where a configuration is a triple encoding the amount of 
  # water in each bucket at a given time-step
  state = intvar(0,max_capacity,shape=(max_step,nb_buckets),name="state")
  goal_step = intvar(1,max_step,name="goal_step") # Objective function

  # For the AUX constraint, see below.
  change = boolvar(shape=(max_step-1,nb_buckets),name="change")

  model = Model(minimize=goal_step)

  for b in buckets:
    # C1: At beginning, buckets contain the amount of water specified by 
    #     function start
    model += [state[0,b]==start[b]]


    # C2: At the end, buckets contain the amount of water specified 
    #     by function goal
    model += [state[goal_step,b] == goal[b]]

  for step in range(max_step-1):
    # C3: The configuration at each step differs from that of the next 
    #     one in that the amount of water of exactly 2 buckets changes 
    #     (that in the others remain the same)
    model += [sum([state[step,b] != state[step+1, b] for b in buckets]) == 2]
    
    # C4: The overall amount of water is the same at each time step
    model += (sum([state[step,b] for b in buckets]) == sum([state[step+1,b] for b in buckets]))
    
    # C5: After each transition (involving buckets b1 and b2), 
    #     either the source bucket becomes empty, or the target becomes full
    for b1 in buckets:
      for b2 in buckets:
        if b1 != b2:
          model += [
            (
            (state[step, b1] != state[step+1, b1]) &
            (state[step, b2] != state[step+1, b2])
            ).implies(
              (state[step+1,b1] == 0)            |
              (state[step+1,b1] == capacity[b1]) |
              (state[step+1,b2] == 0)            |
              (state[step+1,b2] == capacity[b2])
              )
             ]

  # From the second version
  # http:#www.dis.uniroma1.it/~tmancini/index.php?problemid=018&solver=OPL&spec=AUX&currItem=research.publications.webappendices.csplib2x.problemDetails#listing
  # AUX: Addition of auxiliary predicates
  # Auxiliary predicate stores, for each time step, which buckets change 
  # their amount of water wrt the next one
  
  for step in range(max_step-1):
    for b in buckets:
     model += [change[step,b] == (state[step,b] != state[step+1,b])]
  

  # ortools_wrapper(model,sets,print_solution,num_sols)
  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = 0
  if ss.solve() is not False:
    num_solutions += 1
    goal_step_val = goal_step.value()
    state_val = state.value()
    change_val = change.value()*1
    print("goal_step:", goal_step_val)
    print("state:")
    print(state_val[0:goal_step_val])
    print("change:")
    print(change_val[0:goal_step_val])
    print("The solution:")
    for step in range(max_step):
      if step <= goal_step_val:
        print(f"step {step}: {state_val[step]}")
    print(flush=True)

  print("num_solutions:", num_solutions  )
  return num_solutions


# Find all possible combinations that adds to n
# with the restriction that one can take at most
# m elements from each capacity slot.
def comb(capacity=[8,5,3],m=8):
  n = len(capacity)
  x = intvar(0,max(capacity),shape=n,name="x")
  
  model = Model([m == sum(x),
                 [x[i] <= capacity[i] for i in range(n)]
                 ])

  sols = []
  def print_sol():
    xval = x.value()*1
    sols.append(xval)
    
  ss = CPM_ortools(model) 
  ss.solveAll(display=print_sol)
    
  return sols


capacity = [8,5,3]
start   = [8,0,0]
goal    = [4,4,0]
max_step = 10

# Another problem
# capacity = [8,5,3,2]
# start   = [8,0,0,0]
# goal    = [7,1,0,0]
# max_step = 10

water_buckets1(start,goal,capacity,max_step)

# Generate all possible goal states and 
# try to solve the problem.
# Let's increase the max step a notch.
max_step = 10
goal_list = comb(capacity,8)
print("There are",len(goal_list),"(tentative) goal states:")
print(goal_list)
no_solutions = []
for goal in goal_list:
  print("\ngoal:",goal)
  num_solutions = water_buckets1(start,goal,capacity,max_step)
  if num_solutions == 0:
    no_solutions.append(goal)
  print()

print(f"The following {len(no_solutions)} goals has no solution with {max_step} steps:")
print(no_solutions)

# print("\n\nAnother problem:")
# Another problem
# The solution:
# step 0: [9 0 0 0]
# step 1: [1 8 0 0]
# step 2: [1 3 0 5]
# step 3: [0 3 1 5]
# step 4: [5 3 1 0]
#
# capacity = [9,8,7,5]
# start   = [9,0,0,0]
# goal    = [5,3,1,0]
# max_step = 20
#
# water_buckets1(start,goal,capacity,max_step)
