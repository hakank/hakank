"""
Knights tour in cpmpy.

Create a knights path in a n x n matrix for all integers from 1..n*n-1.
The integer n*n is placed whatever it may fit...

This model use the circuit constraints and then we use extract_tour
for getting the proper tour.

Note that the numbers are 0..n*n-1 (since circuit requires that)

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


#
# Extract the tour from the circuit x
#
def extract_tour(x):
    n = len(x)
    k = 0
    tour = np.array([[-1 for i in range(n)] for j in range(n)])
    tour[0][0] = k
    next = x[0,0]
    while k < n*n:
        i = math.floor(next/ n)
        j = (next) % n
        tour[i][j] = k
        next = x[i,j]
        k += 1
    print("Tour:")
    for row in tour:
        print(row)

def knights_tour_circuit(n=4,num_sols=0):

    # Since we use circuit we have to use 0..n*n-1 instead
    x = intvar(0,n*n-1,shape=(n,n),name="x")    
    x_flat = x.flat

    model = Model(
                 AllDifferent(x),
                 Circuit(x_flat),
                 )

    d = [-2,-1,1,2]
    for i in range(n):
        for j in range(n):
            dom = [ (i+a)*n + j+b for a in d for b in d if
                    abs(a) + abs(b) == 3 and
                    i+a >= 0 and i+a < n and
                    j+b >= 0 and j+b < n
                    ]
            model += [member_of(dom,x[i,j])]

    def print_sol():
        x_val = x.value()
        print(x_val)
        extract_tour(x_val)
        print()
        

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print("number of solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()


# Note: this only works for even n
for n in range(6,10+1):
    if n % 2 == 0:
        print("\nn:",n)
        knights_tour_circuit(n,1)

