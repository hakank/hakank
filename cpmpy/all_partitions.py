"""
All partitions in cpmpy.

All integer partitions of a number n.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from collections import defaultdict


def all_partitions(n=4,print_solution=True):
  print("n:",n)
  
  # variables
  x = intvar(0,n,shape=n+1,name="x")

  model = Model([x[0] == 0, # fix x[0] since it's not used
                 n == sum([i*x[i] for i in range(1,n+1)])
                 ])

  def print_sol():
    xs = x.value()
    print([ i for i in range(1,n+1) for _ in range(xs[i]) if xs[i] > 0])

  if print_solution:
    num_solutions = model.solveAll(display=print_sol)
  else:
    num_solutions = model.solveAll()
  print(f"{num_solutions} solutions")

n = 10
all_partitions(n,True)
print()
for n in range(10,41,3):
  all_partitions(n,False)
  print()
