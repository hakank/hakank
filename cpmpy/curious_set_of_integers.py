"""
Crypto problem in cpmpy.

Martin Gardner (February 1967):
'''
The integers 1,3,8, and 120 form a set with a remarkable property: the
product of any two integers is one less than a perfect square. Find
a fifth number that can be added to the set without destroying
this property.
'''

Solution: The number is 0.

There are however other sets of five numbers with this property.
Here are the one in the range of 0.10000:
[0, 1, 3, 8, 120]
[0, 1, 3, 120, 1680]
[0, 1, 8, 15, 528]
[0, 1, 8, 120, 4095]
[0, 1, 15, 24, 1520]
[0, 1, 24, 35, 3480]
[0, 1, 35, 48, 6888]
[0, 2, 4, 12, 420]
[0, 2, 12, 24, 2380]
[0, 2, 24, 40, 7812]
[0, 3, 5, 16, 1008]
[0, 3, 8, 21, 2080]
[0, 3, 16, 33, 6440]
[0, 4, 6, 20, 1980]
[0, 4, 12, 30, 5852]
[0, 5, 7, 24, 3432]
[0, 6, 8, 28, 5460]
[0, 7, 9, 32, 8160]


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def curious_set_of_integers(orig_problem=True):

  model = Model()
  
  # data
  n = 5
  max_val = 10000

  # variables
  x = intvar(0,max_val, shape=n,name="x")

  # constraints
  
  model += [AllDifferent(x)]
  model += [increasing_strict(x)]

  for i in range(n):
    for j in range(n):
      if i != j:
        p = intvar(0, max_val)
        model += [p*p-1 == x[i]*x[j]]

  # This is the original problem:
  # Which is the fifth number?
  if orig_problem:
    v = [1, 3, 8, 120]
    b = boolvar(shape=n)
    model += [sum([
                   x[i] == v[j] for i in range(n) for j in range(len(v))]) == 4
              ]

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 1
  num_solutions = ss.solveAll(display=x)
  print()
  print("num_solutions:", num_solutions)

print("Original problem:")
curious_set_of_integers(True)
print("\nOther solutions:")
curious_set_of_integers(False)
