#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Lectures problem in Z3
#
# Biggs: Discrete Mathematics (2nd ed), page 187.
# '''
# Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
# Among the the potential audience there are people who wish to hear both
#
#  - v1 and v2
#  - v1 and v4
#  - v3 and v5
#  - v2 and v6
#  - v4 and v5
#  - v5 and v6
#  - v1 and v6
#
# How many hours are necessary in order that the lectures can be given
# without clashes?
# '''

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from z3_utils_hakank import *

def main():

  sol = Solver()

  # data

  #
  # The schedule requirements:
  # lecture a cannot be held at the same time as b
  # Note: 1-based
  g = [
      [1, 2],
      [1, 4],
      [3, 5],
      [2, 6],
      [4, 5],
      [5, 6],
      [1, 6]
  ]

  # number of nodes
  n = 6

  # number of edges
  edges = len(g)

  # declare variables
  v = [makeIntVar(sol, 'v[%i]' % i, 0, n - 1) for i in range(n)]

  # maximum color, to minimize
  # Note: since Python is 0-based, the
  # number of colors is +1
  max_c = makeIntVar(sol, "max_c", 0, n - 1)

  #
  # constraints
  #
  maximum(sol,max_c,v)

  # ensure that there are no clashes
  # also, adjust to 0-base
  for i in range(edges):
    sol.add(v[g[i][0] - 1] != v[g[i][1] - 1])

  # symmetry breaking:
  # - v0 has the color 0,
  # - v1 has either color 0 or 1
  sol.add(v[0] == 0)
  sol.add(v[1] <= 1)

  # objective
  # objective = sol.Minimize(max_c, 1)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('max_c:', mod.eval(max_c).as_long() + 1, 'colors')
    print('v:', [mod.eval(v[i]) for i in range(n)])
    print()
    getLessSolution(sol,mod, max_c)

  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
