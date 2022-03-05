#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Place number puzzle in Z3
#
# http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
# '''
# Place numbers 1 through 8 on nodes
# - each number appears exactly once
# - no connected nodes have consecutive numbers
#      2 - 5
#    / | X | \
#  1 - 3 - 6 - 8
#    \ | X | /
#      4 - 7
# ""

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = SolverFor("QF_FD")

  # data
  m = 32
  n = 8
  # Note: this is 1-based for compatibility (and lazyness)
  graph = [
      [1, 2],
      [1, 3],
      [1, 4],
      [2, 1],
      [2, 3],
      [2, 5],
      [2, 6],
      [3, 2],
      [3, 4],
      [3, 6],
      [3, 7],
      [4, 1],
      [4, 3],
      [4, 6],
      [4, 7],
      [5, 2],
      [5, 3],
      [5, 6],
      [5, 8],
      [6, 2],
      [6, 3],
      [6, 4],
      [6, 5],
      [6, 7],
      [6, 8],
      [7, 3],
      [7, 4],
      [7, 6],
      [7, 8],
      [8, 5],
      [8, 6],
      [8, 7]
  ]

  # declare variables
  x = [makeIntVar(sol,"x%i" % i, 1, n) for i in range(n)]

  # constraints
  sol.add(Distinct(x))
  for i in range(m):
    # Note: make 0-based
    sol.add(Abs(
        x[graph[i][0] - 1] - x[graph[i][1] - 1]) > 1)

  # symmetry breaking
  sol.add(x[0] < x[n - 1])

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("x:", [mod.eval(x[i]) for i in range(len(x))])
    getDifferentSolution(sol,mod,x)

  print()
  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  main()
