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


def print_solution(a):
  x = a[0]
  n = len(x)-1
  xs = x.value()
  print([ i for i in range(1,n+1) for _ in range(xs[i]) if xs[i] > 0])
  

def all_partitions(n=4,print_sol=True):
  print("n:",n)
  
  # variables
  x = intvar(0,n,shape=n+1,name="x")

  model = Model([x[0] == 0, # fix x[0] since it's not used
                 n == sum([i*x[i] for i in range(1,n+1)])
                 ])

  if print_sol:
    ortools_wrapper2(model,[x],print_solution)
  else:
    num_solutions=ortools_wrapper_count_solutions(model,[x])
    print(f"{num_solutions} solutions")

n = 10
all_partitions(n,True)

for n in range(10,41,3):
  all_partitions(n,False)
  print()
