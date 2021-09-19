"""
Generate balanced brackets in cpmpy.

This model generates balanced brackets of size m*2.

The number of generated solutions for m:

 m        #
 ----------
  1       1
  2       2
  3       5
  4      14
  5      42
  6     132
  7     429
  8    1430
  9    4862
 10   16796
 11   58786
 12  208012
 13  742900

Which is - of course - the Catalan numbers:
http://oeis.org/search?q=1#2C2#2C5#2C14#2C42#2C132#2C429#2C1430#2C4862#2C16796#2C58786#2C208012&language=english&go=Search
http://oeis.org/A000108


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort


class solution_printer(ort.CpSolverSolutionCallback):
  """
  Solution printer
  """
  def __init__(self, varmap, x, c, print_solutions=True,num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.x_vars = (x)
    self.c_vars = (c)
    self.print_solutions=print_solutions    
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1

    # populate values before printing
    # For array of arrays (Tias' original)
    # for wm in self.vars:
    #   for cpm_var in wm:
    #     cpm_var._value = self.Value(self.varmap[cpm_var])
   
    # For single arrays:
    for cpm_var in self.x_vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    for cpm_var in self.c_vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    (x) = self.x_vars
    (c) = self.c_vars
    if self.print_solutions:
      s = ["[","]"]
      n = len(x)
      print("x:", x.value())
      print("c:", c.value())
      print("cc:", "".join([s[x[i].value()] for i in range(n)]))
      print()

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()

def brackets(m,do_print=False,num_sols=0):
  
    model = Model() 
    n = m*2

    s = ["[","]"]

    # For cumulative (c):
    # +1 if x[i] = "["
    # -1 if x[i] = "]"
    # t = cpm_array([-1,1]) # This don't work
    t = intvar(-1,1,shape=2,name="t")    
    model += (t[0] == 1,t[1] == -1)

    # 0: "[", 1: "]"
    x = boolvar(shape=n,name="x")
    c = intvar(0,n,shape=n,name="c") # counter (cumulative)
    
    # constraints
    
    # start sequence
    model += [x[0] == 0,
              c[0] == 1]

    # cumulative
    for i in range(1,n):
        model += (c[i] == c[i-1] + t[x[i]])

    model += (x[n-1] == 1)
    model += (c[n-1] == 0) # end sequence

    # Redundant constraint: This might make it faster (but it don't)
    model += (sum(x) == m)

    
    ss = CPM_ortools(model)    
    cb = solution_printer(ss.varmap,x,c,do_print,num_sols)
    
    # Flags to experiment with
    # ss.ort_solver.parameters.log_search_progress = True
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0
    
    ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    ss._after_solve(ort_status) # post-process after solve() call...
    if do_print:
      print(ss.status())
      print("Nr solutions:", cb.solcount)
      print("Num conflicts:", ss.ort_solver.NumConflicts())
      print("NumBranches:", ss.ort_solver.NumBranches())
      print("WallTime:", ss.ort_solver.WallTime())
    
    return cb.solcount

brackets(3,True)
# print_sols = True 
print_sols = False
num_sols = []
for i in range(1,11):
  nsols = brackets(i,print_sols)
  print(i, nsols)
  num_sols.append(nsols)

print(num_sols)
