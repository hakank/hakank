"""
Lectures problem in cpmpy.

Biggs: Discrete Mathematics (2nd ed), page 187.
'''
Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
Among the the potential audience there are people who wish to hear both

  - v1 and v2
  - v1 and v4
  - v3 and v5
  - v2 and v6
  - v4 and v5
  - v5 and v6
  - v1 and v6

How many hours are necessary in order that the lectures can be given
without clashes?
'''
 
This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def lectures():


  #
  # The schedule requirements:
  # lecture a cannot be held at the same time as b
  # Note: 1-based
  g = [[1, 2], [1, 4], [3, 5], [2, 6], [4, 5], [5, 6], [1, 6]]

  # number of nodes
  n = 6

  # number of edges
  edges = len(g)

  #
  # declare variables
  #
  v = intvar(0,n-1,shape=n,name="v")

  # maximum color, to minimize
  # Note: since Python is 0-based, the
  # number of colors is +1
  max_c = intvar(0,n-1, name="max_c")

  model = Model(minimize=max_c)

  #
  # constraints
  #
  model += [max_c == max(v)]

  # ensure that there are no clashes
  # also, adjust to 0-base
  for i in range(edges):
    model += [v[g[i][0]-1] != v[g[i][1]-1]]

  # symmetry breaking:
  # - v0 has the color 0,
  # - v1 has either color 0 or 1
  model += [v[0] == 0]
  model += [v[1] <= 1]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("v:", v.value())    
    print("max_c:", max_c.value()+1, "colors")
    print()

  print('num_solutions:', num_solutions)


lectures()
