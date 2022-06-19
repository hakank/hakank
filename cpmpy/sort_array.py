"""
Sorting an array in cpmpy.

Test of the sort_array constraint.

Note: If x contains duplicate values then there will be
      multiple solutions.

Example:
For x = [2,1,2] there are two identical solutions,
since the two 2 are not distinct:
  x: [2 1 2]
  y: [1 2 2]  (permutation: 1,0,2)
                     
  x: [2 1 2]
  y: [1 2 2]  (permutation: 2,0,1)


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,random
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


 
def sort_array_test(n=5):

    # variables
    x = intvar(0,n-1,shape=n,name="x")
    y = intvar(0,n-1,shape=n,name="y")

    # constraints
    model = Model([sort_array(x,y),
                   # x[:4] == [2,1,2,1] # 4 identical solutions (see comment above)
                   # AllDifferent(x) 
                  ])

    def print_sol():
        print("x:",x.value())
        print("y:",y.value())
        print()
    
    num_solutions = model.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)

n = 4
sort_array_test(n)
