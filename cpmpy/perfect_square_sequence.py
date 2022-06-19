"""
Perfect square sequence in cpmpy.

From 'Fun with num3ers'
'Sequence'
http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
'''
If we take the numbers from 1 to 15 
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
and rearrange them in such an order that any two consecutive 
numbers in the sequence add up to a perfect square, we get,

8     1     15     10     6     3     13     12      4      5     11     14        2      7      9
    9    16    25     16     9     16     25     16     9     16     25     16       9     16


I ask the readers the following:

Can you take the numbers from 1 to 25 to produce such an arrangement?
How about the numbers from 1 to 100?
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def is_square(v):
    ub = v.ub # int(math.sqrt(v.Max()))
    z = intvar(1, ub)
    return [z*z == v]

def perfect_square_sequence(n=15, print_solutions=True, show_num_sols=0):
    
    sys.stdout.flush()

    model = Model()

    # data
    print('n: ', n)

    # create the table of possible squares
    squares = []
    for i in range(1, int(math.sqrt(n*n))):
        squares.append(i*i)
    # print("squares:", squares, len(squares))


    # declare variables
    x = intvar(1,n,shape=n,name="x")
    
    # constraints
    model += (AllDifferent(x))
    for i in range(1, n):
        model += (member_of(squares,x[i-1]+x[i]))


    # symmetry breaking
    if n > 1:
        model + (x[0] < x[n-1])

    def print_sol():
        if print_solutions:
            x_val = x.value()
            print("x:", x_val)
            print("diffs:",end=" ")
            for i in range(1, n):
                print((x_val[i-1]+x_val[i]),end=" ")
            print()
        

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0
    
    num_solutions = ss.solveAll(solution_limit=show_num_sols,display=print_sol)
    print("num_solutions:", num_solutions)
    
    return num_solutions

n = 15
if len(sys.argv) > 1:
    n = int(sys.argv[1])

if n == 0:
    sols = []
    for i in range(2,100):
        num = perfect_square_sequence(i, False, 0)
        sols.append((i,num))
    print(sols)
else:
    perfect_square_sequence(n, True, 0)
