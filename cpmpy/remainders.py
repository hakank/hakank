"""
Remainder problem in cpmpy.

'''
11.  Is there a number which when divided by 3 gives a remainder of 1;
when divided by 4, gives a remainder of 2; when divided by 5, gives a
remainder of 3; and when divided by 6, gives a remainder of 4?
(Kordemsky)
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def remainder_problem():
    Max = 10000
    v = intvar(1,Max,shape=5,name="v")
    X,A,B,C,D = v

    model = Model([
        X == A*3 + 1,
        X == B*4 + 2,
        X == C*5 + 3,
        X == D*6 + 4,
        ])

    xs = []
    def print_sol():
        xs.append(v[0].value())

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print(xs)
    print("len:",len(xs))

# Another approach
def remainder_problem2():
    Max = 10000
    v = intvar(1,Max,shape=5,name="v")
    X,A,B,C,D = v

    model = Model()
    for (i,k) in zip(range(1,4+1),[A,B,C,D]):
        model += (X == k*(i+2) + i)

    xs = []
    def print_sol():
        xs.append(v[0].value())
        
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print(xs)
    print("len:",len(xs))

remainder_problem()
print("Another approach")
remainder_problem2()
