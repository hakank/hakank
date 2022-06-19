"""
Knights path in cpmpy.

Create a knights path in a n x n matrix for all integers from 1..n*n-1.
The integer n*n is placed whatever it may fit...

Note: Here we are just calculating a path (not a closing tour).

See knights_tour.py for a faster version that also completes
the tour.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def knights_path(n=4,num_sols=0):

    x = intvar(1,n*n,shape=(n,n),name="x")    
    x_flat = flatten_lists(x)

    model = Model(
                 AllDifferent(x)
                 )

    # From the numbers k = 1 to n*n1,
    #   find the position of k: 
    #    then the position of k+1 must be a knight move
    for k in range(1,n*n):
        i = intvar(0,n-1)
        j = intvar(0,n-1)
        a = intvar(-2,2)
        b = intvar(-2,2)
        
        # 1) First: fix "this" k
        # 2) and then find the position of the next value (k+1)
        model +=[
            # k == x[i,j], # This don't work now
            k == Element(x_flat,i*n+j),
            
            # k + 1 == x[i+a,j+b]
            k + 1 == Element(x_flat,(i+a)*n+j+b),

            i+a >= 0,
            j+b >= 0,
            i+a < n,
            j+b < n,
            a != 0,
            b != 0,
            abs(a)+abs(b) == 3
            ] 


    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=x)
    print("number of solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()


for n in range(2,9):
     print("\nn:",n)
     knights_path(n,1)

