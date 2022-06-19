"""
Simple integer programming problem (Chess set) in cpmpy.

From Applications of Optimization with XPress-MP.pdf
page 11. The problem is presented on page 7.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math,string
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def chess_set():
    small_set = intvar(0,100,name="small_set")
    large_set = intvar(0,100,name="large_set")
    z         = intvar(0,10000,name="z")
        
    model = Model([1*small_set + 3*large_set <= 200,
                   3*small_set + 2*large_set <= 160,
                   z == 5*small_set + 20*large_set,
                   ])


    model.maximize(z)

    def print_sol():
        print("small_set:",small_set.value())
        print("large_set:",large_set.value())        
        print("z:",z.value())
        print()

    ss = CPM_ortools(model)    
    # ss.ort_solver.parameters.max_time_in_seconds = timeout
    # ss.ort_solver.parameters.num_search_workers = num_procs 
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)
    print()

chess_set()
