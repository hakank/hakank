"""
Global constraint all_min_dist in cpmpy.

From Global Constraint Catalogue
http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
'''
Enforce for each pair (vari, varj) of distinct variables of the 
collection VARIABLES that 
|vari - varj| >= MINDIST.

Example
 (2, <5, 1, 9, 3>)

The all_min_dist constraint holds since the following expressions 
|5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
to the first argument MINDIST = 2 of the all_min_dist constraint.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def all_min_dist_test(n=4,c=2):

  model = Model()

  # variables
  x = intvar(1,9,shape=n,name="x")

  # constraints
  model += [all_min_dist(c,x,n)]
  model += [increasing(x)] # symmetry breaking

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:", num_solutions)

n = 4
c = 2
all_min_dist_test(n,c)

