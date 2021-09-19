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

def among(m,x,v):
  """
  among(m,x,v)

  Requires exactly m variables in x to take one of the values in v.
  """
  return [m == sum([x[i] == j for i in range(len(x)) for j in v])]



def among_test():

  n = 5 # length of x
  m = 3 # number of values
  v = [1,5,8]

  # variables
  x = intvar(1,8,shape=n,name="x")

  # constraints  
  model = Model(among(m, x,v))

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = 0
  while ss.solve():
    num_solutions += 1
    print("x:", x.value() )
    get_different_solution(ss,x)

  print("num_solutions:", num_solutions)


among_test()
