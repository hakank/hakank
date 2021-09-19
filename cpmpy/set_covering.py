"""
Set covering in cpmpy.

Placing of firestations, from Winston 'Operations Research', page 486.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *





def set_covering():


  # data
  min_distance = 15
  num_cities = 6

  distance = [
      [0, 10, 20, 30, 30, 20],
      [10, 0, 25, 35, 20, 10],
      [20, 25, 0, 15, 30, 20],
      [30, 35, 15, 0, 15, 25],
      [30, 20, 30, 15, 0, 14],
      [20, 10, 20, 25, 14, 0]
  ]

  # declare variables
  x = boolvar(shape=num_cities,name="x")


  # objective to minimize
  z = intvar(0,10,name="z")

  model = Model(minimize=z)

  # constraints
  model += [z == sum(x) ]

  # ensure that all cities are covered
  for i in range(num_cities):
    model += [sum([x[j] for j in range(num_cities) if distance[i][j] <= min_distance]) >= 1]

  ss = CPM_ortools(model)
  if ss.solve():
    print("z:", z.value())
    print("x:", x.value())
    print()



set_covering()
