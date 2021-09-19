"""
  all_different_except_0 constraint in cpmpy.

  A decomposition of all_different_except_0(x):
  Require that all values in the list x that are not == 0
  are distinct.
  The method all_different_except_0 is defined in cpmpy_hakank.py.
  
  This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
  See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
from cpmpy.solvers.utils import get_supported_solvers
import numpy as np
from cpmpy_hakank import *


def AllDifferent_except_0_test(n,solver=None):
  """
  Test the global constraint AllDifferent_except_0.
  """
  x = IntVar(0,n, shape=n)
  print("x:",x)

  model = Model([
    AllDifferent_except_0(x),
    increasing(x),
    
  ])
  
  print("model:", model)
  num_solutions = 0

  # ortools_wrapper(model,[x])
  ss = CPM_ortools(model)
  print("ss:",ss)
  while ss.solve():
    # print("stat:", model.status())
    num_solutions += 1
    print(x.value())
    get_different_solution(ss,x)
  print("num_solutions:", num_solutions)

# Testing the different solvers
for solver in get_supported_solvers():
  print("\nSolver:", solver)
  AllDifferent_except_0_test(4, solver)
  print()

# AllDifferent_except_0_test(8)
