"""
n-queens problem with degree of diversity of a set of solutions in cpmpy.

The objective is to diversify 9 solutions of 10-queens problem, using two
extra constraint lex_chain_less and soft_alldifferent (defined in this model).

From 
http://www.emn.fr/z-info/sdemasse/gccat/Kdegree_of_diversity_of_a_set_of_solutions.html
'''
  S1=[0,2,5,7,9,4,8,1,3,6],
  S2=[0,3,5,8,2,9,7,1,4,6],
  S3=[1,3,7,2,8,5,9,0,6,4],
  S4=[2,4,8,3,9,6,1,5,7,0],
  S5=[3,6,9,1,4,7,0,2,5,8],
  S6=[5,9,2,6,3,1,8,4,0,7],
  S7=[6,8,1,5,0,2,4,7,9,3],
  S8=[8,1,4,9,7,0,3,6,2,5],
  S9=[9,5,0,4,1,8,6,3,7,2]
  
  The costs associated with the soft_alldifferent_ctr constraints of columns 
  1,2,...,10 are respectively equal to 
  1, 1, 1, 0, 1, 0, 1, 1, 1, and 1. 
'''


Here's is a better - and proven optimal - solution with just 
one not distinct column.

  x:
  [[ 1  8  2  9  6  3 10  4  7  5]
   [ 2  6  3  7 10  8  5  1  4  9]
   [ 3 10  8  1  5  2  6  9  7  4]
   [ 4  9  7  3  1  6  2  5 10  8]
   [ 5  3 10  6  4  9  1  8  2  7]
   [ 6  4  9  5  3 10  7  2  8  1]
   [ 7  1  6  8  2  4  9  3  5 10]
   [ 8  5  1  4  9  7  3 10  6  2]
   [ 9  2  5 10  8  1  4  7  3  6]]
  soft_cost: [0 0 0 0 0 0 0 0 1 0]
  total_cost: 1
  WallTime: 4479.618968184001


  
  For n = 8, the minimal total_cost is 3:
  x:
  [[1 6 8 3 7 4 2 5]
   [2 7 3 6 8 5 1 4]
   [3 5 7 1 4 2 8 6]
   [4 1 5 8 6 3 7 2]
   [4 6 1 5 2 8 3 7]
   [5 2 4 7 3 8 6 1]
   [6 8 2 4 1 7 5 3]]
  soft_cost: [1 1 0 0 0 1 0 0]
  total_cost: 3


  For n=7 the total_cost = 0
  x:
  [[1 6 4 2 7 5 3]
   [3 1 6 4 2 7 5]
   [4 2 7 5 3 1 6]
   [5 3 1 6 4 2 7]
   [6 4 2 7 5 3 1]
   [7 5 3 1 6 4 2]]
  soft_cost: [0 0 0 0 0 0 0]
  total_cost: 0


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations

class solution_printer(ort.CpSolverSolutionCallback):
  """
  Solution printer.
  """
  def __init__(self, varmap, x, soft_cost, total_cost,num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars_x = x
    self.vars_soft_cost = soft_cost
    self.vars_total_cost = total_cost
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1

    # populate values before printing
    # For array of arrays
    for wm in self.vars_x:
      for cpm_var in wm:
        cpm_var._value = self.Value(self.varmap[cpm_var])

    # For single arrays:
    for cpm_var in self.vars_soft_cost:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    for cpm_var in [self.vars_total_cost]:
      cpm_var._value = self.Value(self.varmap[cpm_var])
       
    print("x:")
    print(self.vars_x.value())
    print("soft_cost:",self.vars_soft_cost.value())    
    print("total_cost:",self.vars_total_cost.value())
    print()

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


def queens(x):
  """
  queens(x)

  Ensure that that all placed queens are not in conflict positions.
  """
  n = len(x)
  return [AllDifferent(x),
          AllDifferent([x[i]+i] for i in range(n)),
          AllDifferent([x[i]-i] for i in range(n))]



def queens_diversity(n=10):

  m = n-1
  x = intvar(1,n,shape=(m,n), name="x")

  # costs for soft_all_different
  soft_cost = boolvar(shape=n,name="soft_cost")
  total_cost = intvar(0,n,name="total_cost")

  model = Model([total_cost == soft_cost.sum()])
                
  for j in range(n):
    # soft all different on columns
    model += [soft_alldifferent([x[i,j] for i in range(m)], soft_cost[j])]

  for i in range(m):
    model += [queens(x[i])]

  # symmetry breaking
  model += [lex_chain_less(x)]
  # model += [x[0,0] == 1]

  # Checking (the soft costs from the example shown above)
  # if n == 10:
  #  model += [soft_cost == [1,1,1,0,1,0,1,1,1,1]]

  model.minimize(total_cost)

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.log_search_progress = True
  # ss.ort_solver.parameters.num_search_workers = 12 
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  cb = solution_printer(ss._varmap,x,soft_cost,total_cost)
  ort_status = ss.ort_solver.Solve(ss.ort_model, cb)
  # print(ss._after_solve(ort_status)) # post-process after solve() call...
  print(ss.status())
  print("Nr solutions:", cb.solcount)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())
  


# for n in range(3,9):
#    queens_diversity(n)
n = 10
queens_diversity(n)
