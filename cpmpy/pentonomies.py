"""
Pentominoes in cpmpy.

https://en.wikipedia.org/wiki/Pentomino
'''
A pentomino (or 5-omino) is a polyomino of order 5, that is, a polygon in the
plane made of 5 equal-sized squares connected edge-to-edge. When rotations and
reflections are not considered to be distinct shapes, there are 12 different free
pentominoes. When reflections are considered distinct, there are 18 one-sided
pentominoes. When rotations are also considered distinct, there are 63 fixed pentominoes.
'''
  
This is a port of the MiniZinc benchmark pentominoes_int.mzn 

This model use my decomposition regular which is not very fast.

See pentominoes_problems.py for the instances.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math,string
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

from pentonomies_problems import all_pentonomies_instances


class ORT_pentonomies_printer(ort.CpSolverSolutionCallback):
  """
  Solution printer for pentonomies.
  """
  def __init__(self, varmap, board, height,width, num_solutions=0):
    super().__init__()

    self.solcount = 0
    self.varmap = varmap
    self.vars = (board)
    self.height = height
    self.width = width
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1

    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    alpha = list(string.ascii_lowercase)

    (board) = self.vars
    print(f"#{self.solcount}:")

    height = self.height
    width = self.width
    for h in range(height):
      for w in range(width-1):
        print(alpha[board[(h-1)*width+w].value()],end="")
      print()
    print()

    print(flush=True)        

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


def pentonomies(problem,num_sols=0,timeout=10,num_procs=1):

  width  = problem["width"]
  height = problem["height"]
  filled = problem["filled"]
  ntiles = problem["ntiles"]
  size   = problem["size"]
  tiles  = problem["tiles"]
  dfa    = problem["dfa"]

  Q = 0
  S = 1
  Fstart = 2
  Fend = 3
  Dstart = 4


  # variables
  board = intvar(filled,ntiles+1,shape=width*height,name="board")
  
  model = Model()

  # Note: The data is (partly) 1-based so it's adjusted
  # to 0-based below.
  for h in range(height):
    for w in range(width-1):
      model += [board[(h-1)*width+w] != ntiles+1]

  for h in range(height):
    model += [board[(h-1)*width+width-1] == ntiles+1]

  for t in range(ntiles):
    q = tiles[t][Q] # rows
    s = tiles[t][S] # cols
    f = list(range(tiles[t][Fstart],tiles[t][Fend]+1))

    d = [[0 for _ in range(s)] for _ in range(q) ]
    ii = 0
    jj = 0
    for i in range(tiles[t][Dstart],(tiles[t][Dstart]+q*s)):
      d[ii][jj] = dfa[i]
      if jj == s-1:
         jj = -1
         ii += 1
      jj+= 1
    model += [regular_table(board,q,s,d,1,f)]

  ss = CPM_ortools(model)    
  cb = ORT_pentonomies_printer(ss.varmap,board,height,width,num_sols)

  # Flags to experiment with
  if timeout != None and timeout != 0:
    ss.ort_solver.parameters.max_time_in_seconds = timeout

  if num_sols == 1:
    # Don't work together with SearchForAllSolutions, use Solve(.) instead
    ss.ort_solver.parameters.num_search_workers = num_procs 

  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  if num_sols == 1:
    ort_status = ss.ort_solver.Solve(ss.ort_model, cb)
  else:
    ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
  ss._after_solve(ort_status)
  print(ss.status())
  print("Nr solutions:", cb.solcount)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())


timeout = 10
num_sols = 1
num_procs = 12
print("timeout:",timeout,"num_sols:",num_sols,"num_procs:",num_procs)
for p in all_pentonomies_instances:
  print(f"\nproblem {p}")
  pentonomies(all_pentonomies_instances[p],num_sols,timeout,num_procs)
  
