"""
Knapsack problem in cpmpy.
 
Simple knapsack problem.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def knapsack_model(values, weights, n):
    
    [model, x, z] = knapsack(values, weights, n)
    ss = CPM_ortools(model)
    if ss.solve():
            print("z: ", z.value())
            print("x: ", x.value())
    else:
        print("No solution")
    print()

values =  [15, 100, 90, 60, 40, 15, 10,  1, 12, 12, 100]
weights = [ 2,  20, 20, 30, 40, 30, 60, 10, 21, 12,   2]
n = 102

knapsack_model(values, weights, n)
