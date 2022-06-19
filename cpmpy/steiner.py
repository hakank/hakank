"""
Steiner triplets in cpmpy.

'''
http://www.probp.com/examples/clpset/steiner.pl 
The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements in {1,2,...,n} 
such that each set contains three elements and any two sets have at most one element in common. 
For example, the following shows a solution for size n=7:

     {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}

Problem taken from:
 C. Gervet: Interval Propagation to Reason about Sets: Definition and Implementation of a Practical 
 Language,  Constraints, An International Journal, vol.1, pp.191-246, 1997.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


# number of common elements in two "sets"
def union_card(s1,s2,common):
  return (common == sum([ss1 + ss2 == 2 for ss1,ss2 in zip(s1,s2)]))


def steiner(n=7,num_sols=1):
  
  print("n:",n, "num_sols:", num_sols)
  nb = int(n*(n-1) // 6)

  if not(n % 6 == 1 or n % 6 == 3):
    print("N must be (1|3) modulo 6")
    return()

  sets = boolvar(shape=(nb,n),name="sets")

  model = Model()
  
  # symmetry breaking
  model += (sets[(0,0)] == 1)

  for i in range(nb):
    s1 = [sets[(i,k)] for k in range(n)]
    model += (3 == sum(s1))
    for j in range(nb):
      if i > j:
        # ensure that s1 and s2 has max 1 element in common
        s2 = [sets[(j,k)] for k in range(n)]
        model += (3 == sum(s2))
        # ensure 0..1 in the variable
        common = boolvar(name="common[%i,%i]" % (i,j))
        model += [union_card(s1,s2,common)]
        # model += (common <= 1)

  def print_sol():
    for i in range(nb):
      print([j for j in range(n) if sets[i,j].value() == 1], end=" ")
    print()

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
  if num_sols != 1:
    print("num_solutions:", num_solutions  )
  else:
    print(ss.status)
                
n = 7
if len(sys.argv) > 1:
  n = int(sys.argv[1])
  steiner(n,1)
  print()
else:
  for n in range(3,20):
    if n % 6 == 1 or n % 6 == 3:
      steiner(n,1)
      print()
