"""
Nadel's construction problem in cpmpy.

From Rina Dechter 'Constraint Processing', page 5.
Attributes the problem to
B.A. Nadel 'Constraint satisfaction algorithms' (1989).
'''
* The recreation area should be near the lake.

* Steep slopes are to be avoided for all but the recreation area.
* Poor soil should be avoided for those developments that 
  involve construction, namely the apartments and the family houses.

* The highway, being noisy, should not be near the apartments, 
  the housing, or the recreation area.

* The dumpsite should not be visible from the apartments, 
  the houses, or the lake.

* Lots 3 and 4 have bad soil.
* Lots 3, 4, 7, and 8 are on steep slopes .
* Lots 2, 3, and 4 are near the lake.
* Lots 1 and 2 are near the highway.
'''

Comment: 
I have not found any model that satisfies all the constraints.
However this 'soft' approach counts the broken constraints
and minimizes to 1 broken constraint.

The model (which - of course - could be erroneous) generates 28 different 
solutions. The broken constraints are either
  - steep_slopes constraints or
  - near_dump constraints.



This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def nadel(total_broken_val=None):

  n = 8 # number of lots
  d = 5 # number of developments

  # * Lots 3 and 4 have bad soil.
  # * Lots 3, 4, 7, and 8 are on steep slopes .
  # * Lots 2, 3, and 4 are near the lake.
  # * Lots 1 and 2 are near the highway.
  
                           # 1, 2, 3, 4, 5, 6, 7, 8
  bad_soil     =  cpm_array([0, 0, 1, 1, 0, 0, 0, 0])
  steep_slopes =  cpm_array([0, 0, 1, 1, 0, 0, 1, 1])
  near_lake    =  cpm_array([0, 1, 1, 1, 0, 0, 0, 0])
  near_highway =  cpm_array([1, 1, 0, 0, 0, 0, 0, 0])
 
  # matrix with the proximity of lots
  # (for the dump placement)
  near_lots = np.array([ 
   # 1  2  3  4  5  6  7  8  
    [0, 1, 0, 0, 1, 0, 0, 0], # 1
    [1, 0, 1, 0, 0, 1, 0, 0], # 2 
    [0, 1, 0, 1, 0, 0, 1, 0], # 3 
    [0, 0, 1, 0, 0, 0, 0, 1], # 4
    [1, 0, 0, 0, 0, 1, 0, 0], # 5
    [0, 1, 0, 0, 1, 0, 1, 0], # 6
    [0, 0, 1, 0, 0, 1, 0, 1], # 7
    [0, 0, 0, 1, 0, 0, 1, 0]  # 8
    ])


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
  
  # Array copy of near_lots (for 'matrix element' constraint)
  near_lots_a = boolvar(shape=n*n,name="near_lots")

  #
  # variables
  #


  # the development to place in one of the lots
  recreation = intvar(0,n-1,name="recreation")
  apartments = intvar(0,n-1,name="apartments")
  houses     = intvar(0,n-1,name="houses")
  cemetery   = intvar(0,n-1,name="cemetery")
  dump       = intvar(0,n-1,name="dump")
  developments = [recreation, apartments, houses, cemetery, dump]
  

  c = 13 # number of (potentially) broken constraints (soft constraints)

  broken = boolvar(shape=c,name="broken") # indicator of broken constraint
  total_broken = intvar(0,c,name="total_broken") #  sum(broken)

  # sol.minimize(total_broken)
  if total_broken_val == None:
    model = Model(minimize=total_broken)
  else:
    model = Model(total_broken==total_broken_val)

  for i in range(n):
    for j in range(n):
      model += (near_lots_a[i*n+j] == near_lots[i][j])


  # constraints
  model += (total_broken == sum(broken))
  model += (AllDifferent(developments))

  # * The recreation area should be near the lake.
  model += ((near_lake[recreation] == 1) == ( broken[0] == 0))

  # * Steep slopes are to be avoided for all but the recreation area.
  model += ((steep_slopes[apartments] == 0) == (broken[1] == 0))
  model += ((steep_slopes[houses]     == 0) == (broken[2] == 0))
  model += ((steep_slopes[cemetery]   == 0) == (broken[3] == 0))
  model += ((steep_slopes[dump]       == 0) == (broken[4] == 0))


  # * Poor soil should be avoided for those developments that 
  #   involve construction, namely the apartments and the family houses.
  model += ((bad_soil[apartments] == 0) == (broken[5] == 0))
  model += ((bad_soil[houses]     == 0) == (broken[6] == 0))

  # * The highway, being noisy, should not be near the apartments, 
  #   the housing, or the recreation area.
  model += ((near_highway[apartments] == 0) == (broken[7] == 0))
  model += ((near_highway[houses]     == 0) == (broken[8] == 0))
  model += ((near_highway[recreation] == 0) == (broken[9] == 0))
  
  # The dumpsite should not be visible from the apartments, 
  # the houses, or the lake.

  # not near the lake
  model += ((near_lake[dump] == 0) == (broken[10] == 0))

  # not near the house 
  model += (
    ((near_lots_a[dump*n+houses] == 0) & (near_lots_a[houses*n+dump] == 0)) == (broken[11] == 0)
    ) 

  # not near the apartments  
  model += (
    ((near_lots_a[dump*n+apartments] == 0) & (near_lots_a[apartments*n+dump] == 0)) == (broken[12] == 0)
    )

 
  ss = CPM_ortools(model)
  num_solutions = 0
  if total_broken_val == None:
    if ss.solve():
      num_solutions += 1
      print("developments:", [v.value() for v in developments])
      print("broken constraints:", [i for i in range(c) if broken[i].value()])
      print("total_broken:", total_broken.value())
      print()
      return total_broken.value()
    
  else:
    # Get all the optimal solutions and calculate the number
    # of times a constraints is broken in the solutions.
    broken_constraints_dict = {}
    while ss.solve():
      num_solutions += 1
      print("developments:", [v.value() for v in developments])
      broken_constraints = [i for i in range(c) if broken[i].value()]
      print("broken constraints:", broken_constraints)
      for b in broken_constraints:
        broken_constraints_dict[b] = broken_constraints_dict.get(b,0)+1
      print("total_broken:", total_broken.value())
      print()
      get_different_solution(ss,developments)
      
    print("The broken constraints and their occurrences are:")
    for b in sorted(broken_constraints_dict):
      print(f"Constraint #{b:2d}: {broken_constraints_dict[b]} occurrences")
    
  print()
  print("num_solutions:",num_solutions)

print("Find optimal value:")
total_broken=nadel(None)

print(f"Optimal value is {total_broken}.\nNow find all optimal solutions and the broken constraints:")
nadel(total_broken)
