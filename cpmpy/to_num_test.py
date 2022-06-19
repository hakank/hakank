"""
to_num/3 test in CPMpy.

Model created by Hakan Kjellerstrand, hakank@gmail.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""

from cpmpy import *
from cpmpy.solvers import *
import numpy
from cpmpy_hakank import *


def to_num_test(n=4,base=10):
  base = 10
  n = 4
  x = IntVar(0,base**n-1,name="x")
  a = IntVar(0,base-1, shape=n,name="a")
  
  model = Model([
    to_num(a,x,base),
    # and some arbitrary constraints
    x % 3 == 1,
    x > 5000,
    a[1] + a[2] == a[3],
    AllDifferent(a),
    sum(a) == 22,
    ])
  print("Model:")
  print(model)

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH 
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  def print_sol():
    print("x:",x.value(), "a:",a.value(),flush=True)
  
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

to_num_test(4,10)
