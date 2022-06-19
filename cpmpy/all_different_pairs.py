"""
All different pairs in cpmpy.

Assumption: a is a k by 2 matrix. n is the number of nodes.

This model implements these decompositions:
 - pairs(x,n): function which returns the pairs of matrix x
   in 'integer representation': a[k,1]*(n-1) + a[k,2]

 - all_different_pairs(sol, x,n): all the pairs in x must be different

 - increasing_pairs(sol, x,n): the pairs in x is in increasing order

 - decreasing_pairs(sol, x,n): the pairs in x is in decreasing order

n  #solutions
-------------
1      0
2      1
3     12 
4    377
5  53834
6

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def all_different_pairs_test(n=5):

  model = Model()

  # data
  m = n*(n - 1) // 2 # number of pairs

  print("n:",n, "m:", m)


  # variables
  x = intvar(1,n,shape=(m,2),name="x")

  # constraints

  model += [all_different_pairs(x, n)]
  model += [increasing_pairs(x, n)]

  for k in range(m):
    model += [x[(k,0)] != x[(k,1)]]

  def print_sol():
    for i in range(m):
        for j in range(2):
            print(x[i,j].value(),end=" ")
        print()
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


n = 5
all_different_pairs_test(n)
