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


def alldifferent_except_0_test(n,solver=None):
  """
  Test the global constraint AllDifferent_except_0.
  """
  x = IntVar(0,n, shape=n)
  print("x:",x)

  model = Model([
    all_different_except_0(x),
    increasing(x),
    
  ])
  
  print("model:", model)
  
  num_solutions = model.solveAll(display=x)
  print("num_solutions:", num_solutions)

# Testing the different solvers
print("solvers:", get_supported_solvers())
for solver in get_supported_solvers():
  print("\nSolver:", solver)
  alldifferent_except_0_test(4, solver)
  print()

alldifferent_except_0_test(8)
