"""
Max flow problem in cpmpy.

From Winston 'Operations Research', page 420f, 423f
Sunco Oil example.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def max_flow_winston1():


  #
  # data
  #
  n = 5
  nodes = list(range(n))

  # the arcs
  # Note:
  # This is 1-based to be compatible with other
  # implementations.
  arcs1 = [[1, 2], [1, 3], [2, 3], [2, 4], [3, 5], [4, 5], [5, 1]]

  # convert arcs to 0-based
  arcs = []
  for (a_from, a_to) in arcs1:
    a_from -= 1
    a_to -= 1
    arcs.append([a_from, a_to])

  num_arcs = len(arcs)

  # capacities
  cap = [2, 3, 3, 4, 2, 1, 100]

  # convert arcs to matrix
  # for sanity checking below
  mat = {}
  for i in nodes:
    for j in nodes:
      c = 0
      for k in range(num_arcs):
        if arcs[k][0] == i and arcs[k][1] == j:
          c = 1
      mat[i, j] = c

  #
  # declare variables
  #
  flow = intvar(0,200,shape=(n,n),name="flow")
  z = intvar(0, 10000, name="z")

  model = Model(maximize=z)

  #
  # constraints
  #
  model += [z == flow[n - 1, 0]]

  # capacity of arcs
  for i in range(num_arcs):
    model += [flow[arcs[i][0], arcs[i][1]] <= cap[i]]

  # inflows == outflows
  for i in nodes:
    s1 = sum([
        flow[arcs[k][0], arcs[k][1]] for k in range(num_arcs) if arcs[k][1] == i
    ])
    s2 = sum([
        flow[arcs[k][0], arcs[k][1]] for k in range(num_arcs) if arcs[k][0] == i
    ])
    model += [s1 == s2]

  # sanity: just arcs with connections can have a flow
  for i in nodes:
    for j in nodes:
      if mat[i, j] == 0:
        model += [flow[i, j] == 0]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("z:", z.value())
    for i in nodes:
      for j in nodes:
        print(flow[i, j].value(), end=' ')
      print()
    print()

  print('num_solutions:', num_solutions)


max_flow_winston1()
