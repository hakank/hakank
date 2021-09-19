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


class solution_printer(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for single array printing.
  """
  def __init__(self, varmap, x, cst, num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (x)
    self.cst = cst    
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1 # I always start at 1. :-) 

    # populate values before printing
    # For array of arrays (Tias' original)
    # for wm in self.vars:
    #     for cpm_var in wm:
    #         cpm_var._value = self.Value(self.varmap[cpm_var])
    
    # For single arrays:
    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])
      
    (x) = self.vars

    xx = x.value()
    print("x:", xx, "cst:", self.cst, "y:", xx + self.cst)    
    # print(f"#{self.solcount}: {x.value()}")

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()



def all_different_cst_test():
  # data

  n = 4
  cst = np.array([0,1,0,4]) # the cst shown above
  # cst = [0,0,0,0] # for plain all_different

  x = intvar(1,9,shape=n,name="x")

  # constraints

  model = Model(all_different_cst(x,cst))

  
  # ortools_wrapper(model,[x],print_solution)
  ss = CPM_ortools(model)
  cb = solution_printer(ss.varmap,x,cst,0)
  
  # Flags to experiment with
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
  ss._after_solve(ort_status)
  print(ss.status())
  print("Nr solutions:", cb.solcount)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())


all_different_cst_test()
