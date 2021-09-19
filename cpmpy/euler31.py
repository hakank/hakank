"""
Project Euler problem 31 in cpmpy.
'''
In England the currency is made up of pound, £, and pence, p, and 
there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
'''

This is quite slow. There are much faster ways to solve
this problem. See http://hakank.org/python/euler.py


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

class solution_printer(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for single array printing.
  """
  def __init__(self, varmap, a, num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1 # I always start at 1. :-) 

    # populate values before printing
    # For array of arrays (Tias' original)
    # for wm in self.vars:
    #     for cpm_var in wm:
    #         cpm_var._value = self.Value(self.varmap[cpm_var])
    
    # # For single arrays:
    # for cpm_var in self.vars:
    #   cpm_var._value = self.Value(self.varmap[cpm_var])
      
    # (a) = self.vars            
    # print(f"#{self.solcount}: {a.value()}")

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


def euler31():

   coins = [200,100,50,20,10,5,2,1]
   Max = max(coins)
   n = len(coins)
   x = intvar(0,Max,shape=n,name="x")
   model = Model(200 == sum(coins*x))

   ss = CPM_ortools(model)
   cb = solution_printer(ss.varmap,x,0)

   # Flags to experiment with
   # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
   # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
   # ss.ort_solver.parameters.cp_model_presolve = False
   ss.ort_solver.parameters.linearization_level = 0
   ss.ort_solver.parameters.cp_model_probing_level = 0
   
   ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
   ss._after_solve(ort_status)
   # print(ss.status())
   print("Nr solutions:", cb.solcount)
   # print("Num conflicts:", ss.ort_solver.NumConflicts())
   # print("NumBranches:", ss.ort_solver.NumBranches())
   # print("WallTime:", ss.ort_solver.WallTime())



euler31()
