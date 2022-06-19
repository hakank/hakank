"""
Test of matrix_element in cpmpy.

Note: Using matrix_element(x,i,j,val) is quite slow compared
using
   x_flat = x.flat
   # ...
   model += [val == Element(x_flat,i*cols+j)


The total time difference for a fairly simple Hidato instance is
significant:

  Total time matrix element:  2.2548747062683105
  Total time Element:  0.12157988548278809

Note that the difference in OR-tools's reported solve time
is much less:
  matrix_element: 0.234255119s
  Element      : 0.0322367170s
  
Much of the difference in total times is most probably due to
the amount of work in creating the decomposition of
matrix_element and thus creating a larger model.

Since matrix_element is so slow, I haven't placed it
in cpmpy_hakank.py.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
import time


def matrix_element(x,i,j,val):
    """
    matrix_element(x,i,j,val)

    val = x[i,j]

    Note:
    This is a proof-of-concept (experimental).
    
    It is probably much faster to use the following:
      x_flat = x.flat
      # ....
      val = Element(x_flat,i,j)
      # ...
    """
    n = len(x)
    m = len(x[0])
    constraints = []
    for k in range(n):
        for l in range(m):
            constraints += [((k==i) & (l==j))==(val==x[k,l])]
    # constraints += [sum([x[k,l] == val for k in range(n) for l in range(m)])>= 1]
    return constraints
            

#
# This is a simple Hidato like model.
# See hidato.py for a full model.
# 
def matrix_element_test(problem,method="matrix_element"):

    r = len(problem)
    c = len(problem[0])
   
    x = intvar(1,r*c,shape=(r,c),name="x")
    x_flat = [x[i][j] for i in range(r) for j in range(c)]

    model = Model(
                 AllDifferent(x)
                 )
    
    # Fill in the clues
    for i in range(r):
        for j in range(c):           
            if problem[i][j] > 0:
                model += [x[i,j] == problem[i][j]]
    
    for k in range(1,r*c):
        i = intvar(0,r)
        j = intvar(0,c)
        a = intvar(-1,1)
        b = intvar(-1,1)
        
        # 1) First: fix "this" k
        # 2) and then find the position of the next value (k+1)
        
        if method == "matrix_element":
            model += [matrix_element(x,i,j,k),
                      matrix_element(x,i+a,j+b,k+1)]
        else:
            # This is faster!
            model += [k == Element(x_flat,i*c+j),           
                      k + 1 == Element(x_flat,(i+a)*c+j+b)]

        model += [
            i+a >= 0,
            j+b >= 0,
            i+a < r,
            j+b < c,
            ((a != 0) | (b != 0))
            ] 


    # ss = CPM_ortools(model)
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=x)    
    print("number of solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    
    
# This is a fairly simple Hidato instance
# (See hidato.py for other instances)
problem = [[0,44,41, 0, 0, 0, 0],
           [0,43, 0,28,29, 0, 0],
           [0, 1, 0, 0, 0,33, 0],
           [0, 2,25, 4,34, 0,36],
           [49,16, 0,23, 0, 0, 0],
           [0,19, 0, 0,12, 7, 0],
           [0, 0, 0,14, 0, 0, 0] 
           ]


print("Using matrix_element:")
time1 = time.time()
matrix_element_test(problem,"matrix_element")
time2 = time.time()

print("\n\nUsing Element:")
time3 = time.time()
matrix_element_test(problem,"element")
time4 = time.time()

print("\nTotal time matrix element: ", time2-time1)
print("Total time Element: ", time4-time3)
