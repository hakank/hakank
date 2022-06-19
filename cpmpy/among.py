"""
Global constraint among in cpmpy.
'''
Requires exactly m variables in x to take one of the values in v.
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def among_test():

  n = 5 # length of x
  m = 3 # number of values
  v = [1,5,8]

  # variables
  x = intvar(1,8,shape=n,name="x")

  # constraints  
  model = Model(among(m, x,v))

  ss = CPM_ortools(model)
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:", num_solutions)


among_test()
