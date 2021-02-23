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

  Futoshiki problem in OR-tools CP-SAT Solver.

  From http://en.wikipedia.org/wiki/Futoshiki
  '''
  The puzzle is played on a square grid, such as 5 x 5. The objective
  is to place the numbers 1 to 5 (or whatever the dimensions are)
  such that each row, and column contains each of the digits 1 to 5.
  Some digits may be given at the start. In addition, inequality
  constraints are also initially specifed between some of the squares,
  such that one must be higher or lower than its neighbour. These
  constraints must be honoured as the grid is filled out.
  '''

  Also see
  http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley


  This model is inspired by the Minion/Tailor
  example futoshiki.eprime.

  It's a port of my old CP model futoshiki.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main(values, lt):

  model = cp.CpModel()

  #
  # data
  #
  size = len(values)
  RANGE = list(range(size))
  NUMQD = list(range(len(lt)))

  #
  # variables
  #
  field = {}
  for i in RANGE:
    for j in RANGE:
      field[i, j] = model.NewIntVar(1, size, "field[%i,%i]" % (i, j))
  field_flat = [field[i, j] for i in RANGE for j in RANGE]

  #
  # constraints
  #
  # set initial values
  for row in RANGE:
    for col in RANGE:
      if values[row][col] > 0:
        model.Add(field[row, col] == values[row][col])

  # all rows have to be different
  for row in RANGE:
    model.AddAllDifferent([field[row, col] for col in RANGE])

  # all columns have to be different
  for col in RANGE:
    model.AddAllDifferent([field[row, col] for row in RANGE])

  # all < constraints are satisfied
  # Also: make 0-based
  for i in NUMQD:
    model.Add(
        field[lt[i][0] - 1, lt[i][1] - 1] < field[lt[i][2] - 1, lt[i][3] - 1])

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  
  if status == cp.OPTIMAL:
    for i in RANGE:
      for j in RANGE:
        print(solver.Value(field[i, j]), end=" ")
      print()
    print()

  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


#
# Example from Tailor model futoshiki.param/futoshiki.param
# Solution:
# 5 1 3 2 4
# 1 4 2 5 3
# 2 3 1 4 5
# 3 5 4 1 2
# 4 2 5 3 1
#
# Futoshiki instance, by Andras Salamon
# specify the numbers in the grid
#
values1 = [[0, 0, 3, 2, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0],
           [0, 0, 0, 0, 0]]

# [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
# Note: 1-based
lt1 = [[1, 2, 1, 1], [1, 4, 1, 5], [2, 3, 1, 3], [3, 3, 2, 3], [3, 4, 2, 4],
       [2, 5, 3, 5], [3, 2, 4, 2], [4, 4, 4, 3], [5, 2, 5, 1], [5, 4, 5, 3],
       [5, 5, 4, 5]]

#
# Example from http://en.wikipedia.org/wiki/Futoshiki
# Solution:
# 5 4 3 2 1
# 4 3 1 5 2
# 2 1 4 3 5
# 3 5 2 1 4
# 1 2 5 4 3
#
values2 = [[0, 0, 0, 0, 0], [4, 0, 0, 0, 2], [0, 0, 4, 0, 0], [0, 0, 0, 0, 4],
           [0, 0, 0, 0, 0]]

# Note: 1-based
lt2 = [[1, 2, 1, 1], [1, 4, 1, 3], [1, 5, 1, 4], [4, 4, 4, 5], [5, 1, 5, 2],
       [5, 2, 5, 3]]

if __name__ == "__main__":
  print("Problem 1")
  main(values1, lt1)
  print("\nProblem 2")
  main(values2, lt2)
