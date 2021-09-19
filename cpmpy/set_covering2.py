"""
Set covering in cpmpy.

Example 9.1-2, page 354ff, from
Taha 'Operations Research - An Introduction'
Minimize the number of security telephones in street
corners on a campus.


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def set_covering2():


  # data
  n = 8  # maximum number of corners
  num_streets = 11  # number of connected streets

  # corners of each street
  # Note: 1-based (handled below)
  corner = [[1, 2], [2, 3], [4, 5], [7, 8], [6, 7], [2, 6], [1, 6], [4, 7],
            [2, 4], [5, 8], [3, 5]]

  # declare variables
  x = boolvar(shape=n,name="x")

  z = intvar(0,n,name="z")

  model = Model(minimize=z)
  
  # constraints

  # number of telephones, to be minimized
  model += [z == sum(x)]

  # ensure that all corners are covered
  for i in range(num_streets):
    # also, convert to 0-based
    model += [sum([x[j - 1] for j in corner[i]]) >= 1]


  ss = CPM_ortools(model)
  if ss.solve():
    print("z:", z.value())
    print("x:", x.value())


set_covering2()
