#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Magic squares and cards problem in Z3
#
# Martin Gardner (July 1971)
# '''
# Allowing duplicates values, what is the largest constant sum for an order-3
# magic square that can be formed with nine cards from the deck.
# '''
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main(n=3):

  sol = SimpleSolver()

  #
  # data
  #

  #
  # declare variables
  #
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = makeIntVar(sol, "x(%i,%i)" % (i, j), 1, 13)
  x_flat = [x[(i, j)] for i in range(n) for j in range(n)]

  s = makeIntVar(sol,"s", 1, 13 * 4)
  counts = [makeIntVar(sol, "counts(%i)" % i, 0, 4) for i in range(14)]

  #
  # constraints
  #
  global_cardinality_count(sol, list(range(14)), x_flat, counts)

  # the standard magic square constraints (sans all_different)
  [sol.add(Sum([x[(i, j)] for j in range(n)]) == s) for i in range(n)]
  [sol.add(Sum([x[(i, j)] for i in range(n)]) == s) for j in range(n)]

  sol.add(Sum([x[(i, i)] for i in range(n)]) == s)  # diag 1
  sol.add(Sum([x[(i, n - i - 1)] for i in range(n)]) == s)  # diag 2

  # redundant constraint
  sol.add(Sum(counts) == n * n)

  # objective
  # sol.maximize(s)

  #
  # solution and search
  #
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("s:", mod.eval(s))
    print("counts:", [mod.eval(counts[i]) for i in range(14)])
    for i in range(n):
      for j in range(n):
        print("%2i" % mod.eval(x[(i, j)]).as_long(), end=' ')
      print()
    print()
    getGreaterSolution(sol,mod,s)

  print()
  print("num_solutions:", num_solutions)


n = 3
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  main(n)
