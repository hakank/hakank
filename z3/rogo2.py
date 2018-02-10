#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Rogo puzzle solver in Z3
#
# From http://www.rogopuzzle.co.nz/
# '''
# The object is to collect the biggest score possible using a given
# number of steps in a loop around a grid. The best possible score
# for a puzzle is given with it, so you can easily check that you have
# solved the puzzle. Rogo puzzles can also include forbidden squares,
# which must be avoided in your loop.
# '''

# Also see Mike Trick:
# 'Operations Research, Sudoko, Rogo, and Puzzles'
# http://mat.tepper.cmu.edu/blog/?p=1302
#
#  Problem instances:
#  * http://www.hakank.org/z3/rogo_mike_trick.py
#  * http://www.hakank.org/z3/rogo_20110106.py
#  * http://www.hakank.org/z3/rogo_20110107.py
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
import re
from z3_utils_hakank import *


def main(problem, rows, cols, max_steps):

  sol = Solver()

  # data
  W = 0
  B = -1
  print("rows: %i cols: %i max_steps: %i" % (rows, cols, max_steps))

  problem_flatten = [problem[i][j] for i in range(rows) for j in range(cols)]
  max_point = max(problem_flatten)
  print("max_point:", max_point)
  max_sum = sum(problem_flatten)
  print("max_sum:", max_sum)
  print()

  # variables

  # an Array version of problem_flatten (for the element constraints)
  problem_flatten_a = makeIntArray(sol,"problem_flatten_a", rows*cols, -1,max_point)
  for i in range(rows*cols):
    sol.add(problem_flatten_a[i] == problem_flatten[i])

  # the coordinates
  x = [makeIntVar(sol, "x[%i]" % i, 0, rows - 1) for i in range(max_steps)]
  y = [makeIntVar(sol, "y[%i]" % i, 0, cols - 1) for i in range(max_steps)]

  # the collected points
  points = [makeIntVar(sol, "points[%i]" % i, 0, max_point)
            for i in range(max_steps)]

  # objective: sum of points in the path
  sum_points = makeIntVar(sol, "sum_points", 0, max_sum)

  # constraints

  # all coordinates must be unique
  for s in range(max_steps):
    for t in range(s + 1, max_steps):
      # b1 = x[s] != x[t]
      # b2 = y[s] != y[t]
      # sol.add(b1 + b2 >= 1)
      sol.add(Or(x[s] != x[t], y[s] != y[t]))

  # calculate the points (to maximize)
  for s in range(max_steps):
    sol.add(points[s] == problem_flatten_a[x[s] * cols + y[s]])

  sol.add(sum_points == sum(points))

  # ensure that there are not black cells in
  # the path
  for s in range(max_steps):
    sol.add(problem_flatten_a[x[s] * cols + y[s]] != B)

  # get the path
  for s in range(max_steps - 1):
    sol.add(Abs(x[s] - x[s + 1]) + Abs(y[s] - y[s + 1]) == 1)

  # close the path around the corner
  sol.add(Abs(x[max_steps - 1] - x[0]) + Abs(y[max_steps - 1] - y[0]) == 1)

  # symmetry breaking: the cell with lowest coordinates
  # should be in the first step.
  for i in range(1, max_steps):
    sol.add(x[0] * cols + y[0] < x[i] * cols + y[i])

  # symmetry breaking: second step is larger than
  # first step
  # sol.add(x[0]*cols+y[0] < x[1]*cols+y[1])

  #
  # objective
  #
  # sol.maximize(sum_points)

  # for a in sol.assertions():
  #    print(a)
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("sum_points:", mod.eval(sum_points))
    print("adding 1 to coords...")
    for s in range(max_steps):
      print("%i %i" % (mod.eval(x[s]).as_long() + 1, mod.eval(y[s]).as_long() + 1))
    print()
    getGreaterSolution(sol,mod,sum_points)

  print("\nnum_solutions:", num_solutions)


# Default problem:
# Data from
# Mike Trick: "Operations Research, Sudoko, Rogo, and Puzzles"
# http://mat.tepper.cmu.edu/blog/?p=1302
#
# This has 48 solutions with symmetries;
# 4 when the path symmetry is removed.
#
rows = 5
cols = 9
max_steps = 12
W = 0
B = -1
problem = [
    [2, W, W, W, W, W, W, W, W],
    [W, 3, W, W, 1, W, W, 2, W],
    [W, W, W, W, W, W, B, W, 2],
    [W, W, 2, B, W, W, W, W, W],
    [W, W, W, W, 2, W, W, 1, W]
]
if __name__ == "__main__":
  if len(sys.argv) > 1:
    exec(compile(open(sys.argv[1]).read(), sys.argv[1], 'exec'))
  main(problem, rows, cols, max_steps)
