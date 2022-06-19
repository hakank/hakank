"""
Number square problem in cpmpy.

From Pascal Van Henrentyck 'The OPL Optimization Programming Language', 
page 32:
'''
Consider finding an eight digit number that is a square and remains a square
when 1 is concatenated in front of its decimal notation.
'''

There are two solutions:

  n = 23765625
  x = 4875
  y = 11125

  n = 56250000
  x = 7500
  y = 12500



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def number_square():

    
    n = intvar(10000000,99999999,name="n")
    x = intvar(0,20000,name="x")
    y = intvar(0,20000,name="y")

    model = Model([n == x*x, 100000000+n ==y*y]
                  )

    def print_sol():
        print("n:",n.value())        
        print("x:", x.value())
        print("y:", y.value())        
        print()


    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0
    num_solutions = ss.solveAll(display=print_sol)
    print("number of solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())

number_square()
