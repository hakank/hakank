#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Max flow problem in Z3
#
# From Winston 'Operations Research', page 420f, 423f
# Sunco Oil example.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main():

  # Create the solver.
  sol = SolverFor("QF_FD")

  #
  # data
  #
  n = 5
  nodes = list(range(n))

  # the arcs
  # Note:
  # This is 1-based to be compatible with other
  # implementations.
  arcs1 = [
      [1, 2],
      [1, 3],
      [2, 3],
      [2, 4],
      [3, 5],
      [4, 5],
      [5, 1]
  ]

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
  flow = {}
  for i in nodes:
    for j in nodes:
      flow[i, j] = makeIntVar(sol,'flow %i %i' % (i, j), 0, 200)

  flow_flat = [flow[i, j] for i in nodes for j in nodes]

  z = makeIntVar(sol, "z", 0, 10000)

  #
  # constraints
  #
  sol.add(z == flow[n - 1, 0])

  # capacity of arcs
  for i in range(num_arcs):
    sol.add(flow[arcs[i][0], arcs[i][1]] <= cap[i])

  # inflows == outflows
  for i in nodes:
    sol.add(Sum([flow[arcs[k][0], arcs[k][1]]
                 for k in range(num_arcs) if arcs[k][1] == i])
            ==
            Sum([flow[arcs[k][0], arcs[k][1]]
                 for k in range(num_arcs) if arcs[k][0] == i])
            )
    

  # sanity: just arcs with connections can have a flow
  for i in nodes:
    for j in nodes:
      if mat[i, j] == 0:
        sol.add(flow[i, j] == 0)

  # objective: maximize z
  # sol.maximize(z)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('z:', mod.eval(z))
    for i in nodes:
      for j in nodes:
        print(mod.eval(flow[i, j]), end=' ')
      print()
    print()
    getGreaterSolution(sol,mod,z)

  print('num_solutions:', num_solutions)

if __name__ == '__main__':
  main()
