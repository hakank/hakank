"""
Magic squares in cpmpy

See https://en.wikipedia.org/wiki/Magic_square


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def magic_square(n=4,num_sols=0,symmetry_breaking=False,num_procs=1):
    print(f"\n\nn:{n} num_sols:{num_sols}")

    m = n*n
    x = intvar(1,m,shape=(n, n), name='x')
    x_flat = x.flat
    
    total = math.ceil(n*(m+1)/2)
    print("total:",total)
    
    model = Model (
        [
        AllDifferent(x),
        [ sum(row) == total for row in x],
        [ sum(col) == total for col in x.transpose()],               
        # sum([ x[i,i] for i in range(n)]) == total, # diag 1
        sum(x.diagonal()) == total,
        sum([ x[i,n-i-1] for i in range(n)]) == total, # diag 2
        ]
        )

    if symmetry_breaking:
        model += [frenicle(x,n)]

    def print_sol():
        print(x.value())
    
    ss = CPM_ortools(model)
    # Flags to experiment with        
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0
    
    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print("Nr solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print(flush=True)

symmetry_breaking=True
# n=3: 8 solutions w/o symmetry breaking
# With symmetry breaking: 1 solution
magic_square(3,0,symmetry_breaking)

# 880 solutions with symmetry breaking
# magic_square(4,0,symmetry_breaking)

# Just first solution (it's faster without symmetry breaking).
symmetry_breaking=False
num_sols = 1
num_procs = 8
for n in range(3,15+1):
    magic_square(n,num_sols,symmetry_breaking,num_procs)


