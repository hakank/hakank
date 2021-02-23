# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""
  Rogo puzzle solver in OR-tools CP-SAT Solver.

  From http://www.rogopuzzle.co.nz/
  '''
  The object is to collect the biggest score possible using a given
  number of steps in a loop around a grid. The best possible score
  for a puzzle is given with it, so you can easily check that you have
  solved the puzzle. Rogo puzzles can also include forbidden squares,
  which must be avoided in your loop.
  '''

  Also see Mike Trick:
  'Operations Research, Sudoko, Rogo, and Puzzles'
  http://mat.tepper.cmu.edu/blog/?p=1302

  Problem instances:
  * http://www.hakank.org/or_tools/rogo_mike_trick.py
  * http://www.hakank.org/or_tools/rogo_20110106.py
  * http://www.hakank.org/or_tools/rogo_20110107.py

  This is a port of my old CP model rogo2.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/

"""
from __future__ import print_function
import re
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main(problem, rows, cols, max_steps):

  model = cp.CpModel()

  #
  # data
  #
  W = 0
  B = -1
  print("rows: %i cols: %i max_steps: %i" % (rows, cols, max_steps))

  problem_flatten = [problem[i][j] for i in range(rows) for j in range(cols)]
  max_point = max(problem_flatten)
  print("max_point:", max_point)
  max_sum = sum(problem_flatten)
  print("max_sum:", max_sum)
  print()

  #
  # declare variables
  #

  # the coordinates
  x = [model.NewIntVar(0, rows - 1, "x[%i]" % i) for i in range(max_steps)]
  y = [model.NewIntVar(0, cols - 1, "y[%i]" % i) for i in range(max_steps)]

  # the collected points
  points = [
      model.NewIntVar(0, max_point, "points[%i]" % i) for i in range(max_steps)
  ]

  # objective: sum of points in the path
  sum_points = model.NewIntVar(0, max_sum, "sum_points")

  #
  # constraints
  #

  # all coordinates must be unique
  for s in range(max_steps):
    for t in range(s + 1, max_steps):
      b1 = model.NewBoolVar("b1")
      b2 = model.NewBoolVar("b1")
      model.Add(x[s] != x[t]).OnlyEnforceIf(b1)
      model.Add(y[s] != y[t]).OnlyEnforceIf(b2)
      model.Add(b1 + b2 >= 1)

  # calculate the points (to maximize)
  for s in range(max_steps):
    ix = model.NewIntVar(0,len(problem_flatten),"ix")
    model.Add(ix == x[s] * cols + y[s])
    model.AddElement(ix, problem_flatten, points[s])

  model.Add(sum_points == sum(points))

  # ensure that there are not black cells in
  # the path
  for s in range(max_steps):
    ix = model.NewIntVar(0,len(problem_flatten),"ix")
    model.Add(ix == x[s] * cols + y[s])
    val = model.NewIntVar(0,100, "val")
    model.AddElement(ix, problem_flatten, val)
    model.Add(val != B)

  # get the path
  for s in range(max_steps - 1):
    t1 = model.NewIntVar(-100,100, "t1")
    model.Add(t1 == x[s] - x[s + 1])
    t2 = model.NewIntVar(-100,100, "t1")
    model.Add(t2 == y[s] - y[s + 1])
    t1_abs = model.NewIntVar(0,100, "t1_abs")
    model.AddAbsEquality(t1_abs,t1)
    t2_abs = model.NewIntVar(0,100, "t2_abs")
    model.AddAbsEquality(t2_abs,t2)
    model.Add(t1_abs + t2_abs == 1)

  # close the path around the corner
  end_t1 = model.NewIntVar(-100,100, "t1")
  model.Add(end_t1 == x[max_steps - 1] - x[0])
  end_t2 = model.NewIntVar(-100,100, "t1")
  model.Add(end_t2 == y[max_steps - 1] - y[0])
  end_t1_abs = model.NewIntVar(0,100, "t1_abs")
  model.AddAbsEquality(end_t1_abs,end_t1)
  end_t2_abs = model.NewIntVar(0,100, "t2_abs")
  model.AddAbsEquality(end_t2_abs,end_t2)
  model.Add(end_t1_abs + end_t2_abs == 1)


  # symmetry breaking: the cell with lowest coordinates
  # should be in the first step.
  for i in range(1, max_steps):
    model.Add(x[0] * cols + y[0] < x[i] * cols + y[i])

  # symmetry breaking: second step is larger than
  # first step
  # model.Add(x[0]*cols+y[0] < x[1]*cols+y[1])

  #
  # objective
  #
  model.Maximize(sum_points)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  status = solver.Solve(model)
  
  if status == cp.OPTIMAL:
    print("sum_points:", solver.Value(sum_points))
    print("adding 1 to coords...")
    for s in range(max_steps):
      print("%i %i" % (solver.Value(x[s]) + 1, solver.Value(y[s]) + 1))
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


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
problem = [[2, W, W, W, W, W, W, W, W], [W, 3, W, W, 1, W, W, 2, W],
           [W, W, W, W, W, W, B, W, 2], [W, W, 2, B, W, W, W, W, W],
           [W, W, W, W, 2, W, W, 1, W]]
if __name__ == "__main__":
  if len(sys.argv) > 1:
    exec(compile(open(sys.argv[1]).read(), sys.argv[1], "exec"))
  main(problem, rows, cols, max_steps)
