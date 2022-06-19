"""
Global constraint all_different_cst in cpmpy

From Global Constraint Catalog:
http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_cst.html
'''
For all pairs of items (VARIABLES[i], VARIABLES[j]) (i!=j) of the 
collection VARIABLES enforce 
VARIABLES[i].var+VARIABLES[i].cst != VARIABLES[j].var+VARIABLES[j].cst.

Example
 (<
    var-5 cst-0,
    var-1 cst-1,
    var-9 cst-0,
    var-3 cst-4
 >
 )

The alldifferent_cst constraint holds since all the expressions 
5+0=5, 1+1=2, 9+0=9 and 3+4=7 correspond to distinct values.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def all_different_cst_test():
  # data

  n = 4
  cst = np.array([0,1,0,4]) # the cst shown above
  # cst = [0,0,0,0] # for plain all_different

  x = intvar(1,9,shape=n,name="x")

  # constraints

  model = Model(all_different_cst(x,cst))

  sol_count = [0]
  def print_sol():
    sol_count[0] += 1
    xx = x.value()
    print(f"sol #{sol_count[0]} x {xx} cst: {cst} y: {xx + cst}")
    
  ss = CPM_ortools(model) 
  # Flags to experiment with
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  num_solutions = ss.solveAll(display=print_sol)
  print("Nr solutions:", num_solutions)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())


all_different_cst_test()
