"""
Mastermind like problem in cpmpy.

From https://puzzling.stackexchange.com/questions/97032/5-digit-puzzle-code-looking-for-solution
'''
4 7 2 9 1 - One number is correct but not in right position
9 4 6 8 7 - One number is correct but not in right position
3 1 8 7 2 - Two numbers are correct but only one is in right position
1 5 7 3 9 - Two numbers are correct and both in right position

Assuming all the digits are distinct, what is the 5-digit number?
'''

This model is a port of (and improved) the Z3 model in
https://g-ar.github.io/posts/solving-mastermind-like-problems-using-z3-theorem-prover/
Solving Mastermind-like Problems Using Z3 Theorem Prover

Solution: [6, 5, 0, 3, 2]

Cf number_lock.py.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

#
# A fairly faithful port of the Z3 in op.cit.
# though I (hakank) moved the correct number
# constraints to the proper model.
#
def mastermind_like_problem():
    
    cols = 5
    a = [[4,7,2,9,1],
         [9,4,6,8,7],
         [3,1,8,7,2],
         [1,5,7,3,9]]

    # variables
    x = intvar(0,9,shape=cols,name="x")

    # constraints
    '''
    All are distinct
    '''
    model = Model(AllDifferent(x))

    model += [sum([x[c] != a[0][c] for c in range(cols)]) == 5, # no number in correct position
              sum([x[c] != a[1][c] for c in range(cols)]) == 5, # no number in correct position
              sum([x[c] != a[2][c] for c in range(cols)]) == 4, # one number in correct position
              sum([x[c] != a[3][c] for c in range(cols)]) == 3  # two numbers in correct position
              ]

    # hakank: I moved the "hack in the solution part to proper constraints
    model += [sum([x[c] == a[0][d] for c in range(cols) for d in range(cols)]) == 1, # 1 number is correct
              sum([x[c] == a[1][d] for c in range(cols) for d in range(cols)]) == 1, # 1 number is correct
              sum([x[c] == a[2][d] for c in range(cols) for d in range(cols)]) == 2, # 2 numbers are correct
              sum([x[c] == a[3][d] for c in range(cols) for d in range(cols)]) == 2  # 2 numbers are correct
              ]

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=x)
    print("num_solutions: ", num_solutions)
    print()    

#
# hakank: This is a more general approach.
#
def mastermind_like_problem2(a,correct_position,correct_number):
    
    rows = len(a)
    cols = len(a[0])    

    # variables
    x = intvar(0,9,shape=cols,name="x")

    # constraints
    model = Model(AllDifferent(x))

    for i in range(rows):
        model += [sum([x[c] == a[i][c] for c in range(cols)]) == correct_position[i]]
        model += [sum([x[c] == a[i][d] for c in range(cols) for d in range(cols)]) == correct_number[i]]

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=x)
    print("num_solutions: ", num_solutions)
    print()

mastermind_like_problem()


a = [[4,7,2,9,1],
     [9,4,6,8,7],
     [3,1,8,7,2],
     [1,5,7,3,9]]
correct_position = [0,0,1,2]
correct_number   = [1,1,2,2]
mastermind_like_problem2(a,correct_position,correct_number)

