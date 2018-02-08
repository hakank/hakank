#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Assignment problem in Z3
#
# Winston 'Operations Research', Assignment Problems, page 393f
# (generalized version with added test column)
#
# Note: some of the refinements in the original Google or-tools model
# was done by Laurent Perron (of Google or-tools fame).
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *

def main(cost, rows, cols):

  sol = Optimize()

  # data

  # declare variables
  total_cost = makeIntVar(sol,"total_cost", 0,100)
  x = []
  for i in range(rows):
    t = []
    for j in range(cols):
      t.append(makeIntVar(sol,"x[%i,%i]" % (i, j),0,1))
    x.append(t)
  x_flat = [x[i][j] for i in range(rows) for j in range(cols)]

  # constraints

  # total_cost
  sol.add(
      total_cost == Sum(
              [scalar_product2(sol,x_row, cost_row)
               for (x_row, cost_row) in zip(x, cost)]))

  # exacly one assignment per row, all rows must be assigned
  [sol.add(Sum([x[row][j] for j in range(cols)]) == 1) for row in range(rows)]

  # zero or one assignments per column
  [sol.add(Sum([x[i][col] for i in range(rows)]) <= 1) for col in range(cols)]

  sol.minimize(total_cost)

  # solution and search
  num_solutions = 0
  if sol.check()==sat:
    mod = sol.model()
    print("total_cost:", mod.eval(total_cost))
    for i in range(rows):
      for j in range(cols):
        print(mod.eval(x[i][j]), end=' ')
      print()
    print()

    for i in range(rows):
      print("Task:", i, end=' ')
      for j in range(cols):
        if mod.eval(x[i][j]).as_long() == 1:
          print(" is done by ", j)
    print()


# Problem instance
# hakank: I added the fifth column to make it more
#         interesting
rows = 4
cols = 5
cost = [[14, 5, 8, 7, 15],
        [2, 12, 6, 5, 3],
        [7, 8, 3, 9, 7],
        [2, 4, 6, 10, 1]]

if __name__ == "__main__":
  main(cost, rows, cols)
