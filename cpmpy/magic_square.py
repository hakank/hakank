"""
Magic squares in cpmpy

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def magic_square(n=4,num_sols=0,num_procs=1):
    print(f"\n\nn:{n} num_sols:{num_sols}")

    m = n*n
    x = intvar(1,m,shape=(n, n), name='x')
    x_flat = [x[i][j] for i in range(n) for j in range(n)]
    
    total = math.ceil(n*(m+1)/2)
    print("total:",total)
    
    model = Model (
        [
        AllDifferent(x),
        [ sum(row) == total for row in x],
        [ sum(col) == total for col in x.transpose()],               
        sum([ x[i,i] for i in range(n)]) == total, # diag 1
        sum([ x[i,n-i-1] for i in range(n)]) == total, # diag 2
        ]
        )
    
    s = CPM_ortools(model)
    # Note that we have to use a flattened version of x.
    cb = ORT_simple_printer_matrix(s.varmap,x_flat,n,n,num_sols)

    if num_sols == 1:
        print("number of processes:", num_procs)
        s.ort_solver.parameters.num_search_workers = num_procs

    # Flags to experiment with        
    # s.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # s.ort_solver.parameters.cp_model_presolve = False
    s.ort_solver.parameters.linearization_level = 0
    s.ort_solver.parameters.cp_model_probing_level = 0

    if num_sols > 1:
        ort_status = s.ort_solver.SearchForAllSolutions(s.ort_model, cb)
    else:
        ort_status = s.ort_solver.Solve(s.ort_model, cb)
    print("After solve status:", s._after_solve(ort_status)) # post-process after solve() call...
    print("s.status():", s.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", s.ort_solver.NumConflicts())
    print("NumBranches:", s.ort_solver.NumBranches())
    print("WallTime:", s.ort_solver.WallTime())

# All 8 solutions for n=3
magic_square(3)

# Just first solution
num_sols = 1
num_procs = 8
for n in range(3,15+1):
    magic_square(n,num_sols,num_procs)


