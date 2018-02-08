#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Knapsack problem in Z3
#
# Simple knapsack problem.
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def knapsack(sol, values, weights, n):
  z = makeIntVar(sol, "z", 0, 10000)
  x = [makeIntVar(sol, "x(%i)" % i, 0, 1) for i in range(len(values))]
  sol.add(z >= 0)
  sol.add(z == scalar_product2(sol, x, values))
  sol.add(scalar_product2(sol, x, weights) <= n)

  return [x, z]


def main(values, weights, n):

  sol = Solver()

  #
  # data
  #
  print("values:", values)
  print("weights:", weights)
  print("n:", n)
  print()

  # declare variables

  #
  # constraints
  #
  [x, z] = knapsack(sol, values, weights, n)

  # objective
  # sol.maximize(z)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("x:", [mod.eval(x[i]) for i in range(len(values))])
    print("z:", mod.eval(z))
    print()
    getGreaterSolution(sol,mod,z)

  print()
  print("num_solutions:", num_solutions)


values = [15, 100, 90, 60, 40, 15, 10, 1, 12, 12, 100]
weights = [2, 20, 20, 30, 40, 30, 60, 10, 21, 12, 2]
n = 102

if __name__ == "__main__":
  main(values, weights, n)
