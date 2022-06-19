"""
Global constraint all_different_on_intersection in cpmpy.

From Global Constraint Catalogue
http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_on_intersection.html
'''
The values that both occur in the VARIABLES1 and VARIABLES2 collections 
have only one occurrence.

Example
(
 <5, 9, 1, 5>,
 <2, 1, 6, 9, 6, 2>
)

The alldifferent_on_intersection constraint holds since the values 9 and 1 
that both occur in <5, 9, 1, 5> as well as in <2, 1, 6, 9, 6, 2> have 
exactly one occurrence in each collection.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def all_different_on_intersection_test():
  
  model = Model()
  m = 4
  n = 6

  x_init = [5,9,-1,5]
  y_init = [2,1,6,9,6,2]

  # variables
  x = intvar(1,9,shape=m,name="x")
  y = intvar(1,9,shape=n,name="y")  

  for i in range(m):
    if x_init[i] >= 0:
        model += [x[i] == x_init[i]]

  for i in range(n):
    model += [y[i] == y_init[i]]

  # constraints
  model += [all_different_on_intersection(x,y)]

  def print_sol():
    print("x:", x.value())
    print("y:", y.value())    
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


all_different_on_intersection_test()
