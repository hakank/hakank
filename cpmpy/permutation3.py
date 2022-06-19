"""
Test of permutation3 constraint in cpmpy

permutation(x,p,y) ensures that the array y
is a permutation of array x with the permutation
operations in array p. This means:
   y[i] = x[p[i]] 

Example:
  x: [3 1 2 4]
  p: [2 1 3 0]
  y: [2 1 4 3]
  i:0 y[0] = x[p[0]] = x[2] = 2
  i:1 y[1] = x[p[1]] = x[1] = 1
  i:2 y[2] = x[p[2]] = x[3] = 4
  i:3 y[3] = x[p[3]] = x[0] = 3


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def permutation3_test(n=5):

  # variables
  x = intvar(1,n,shape=n,name="x")
  p = intvar(0,n-1,shape=n,name="p")
  y = intvar(1,n,shape=n,name="y")  
  
  # constraints
  model = Model([AllDifferent(x),
                 AllDifferent(p),
                 AllDifferent(y),
                 permutation3(x,p,y)

                 # Some experiments (beware of the domains of x and y)
                 # x == list(range(n)),
                 # x == [2,0,1,3], # 0..n-1
                 # x == [3,1,2,4], # 1..n
                 # p == [0,1,2], # x and y are the same
                 # x == y, # -> p = [0,1,...,n-1]
                 
                 # permutation(x,y) # no explicit permutation array p
                 ])

  def print_sol():
    print("x:",x.value())
    print("p:",p.value())
    print("y:",y.value())
    for i in range(len(x)):
      print(f"i:{i} y[{i}] = x[p[{i}]] = x[{p[i].value()}] = {x[p[i]].value()}")
    print()
    
  num_sols=0
  if n >= 100:
    num_sols = 1
    
  num_solutions = model.solveAll(solution_limit=num_sols,display=print_sol)

n = 4
permutation3_test(n)


