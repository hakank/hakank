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
  Coins grid problem in Google OR-tools CP-SAT solver.

  Problem from
  Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  '''
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one should
  place coins in such a way that the following conditions are fulfilled:
     1. In each row exactly 14 coins must be placed.
     2. In each column exactly 14 coins must be placed.
     3. The sum of the quadratic horizontal distance from the main diagonal
        of all cells containing a coin must be as small as possible.
     4. In each cell at most one coin can be placed.
  The description says to place 14x31 = 434 coins on the chessboard each row
  containing 14 coins and each column also containing 14 coins.
  '''

  Cf the LPL model:
  http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin

  Note: Laurent Perron helped me to improve this model.

  This is a port of my old OR-tools CP model.
  
  It is _much_ faster than the old CP model and is on par
  with the MIP model.

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main(n=31, c=14):
  model = cp.CpModel() 

  # data
  print("n: ", n)
  print("c: ", c)

  # declare variables
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = model.NewBoolVar("x %i %i" % (i, j))

  #
  # constraints
  #

  # sum rows/columns == c
  for i in range(n):
    model.Add(sum([x[(i, j)] for j in range(n)]) == c)  # sum rows
    model.Add(sum([x[(j, i)] for j in range(n)]) == c)  # sum cols

  # quadratic horizonal distance var
  obj = model.NewIntVar(0,n*n*c*c,"obj") 
  model.Add(obj == sum([x[(i, j)] * (i - j) * (i - j) for i in range(n) for j in range(n)]))

  # objective
  model.Minimize(obj)

  # Search and solution
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("obj:", solver.Value(obj))
    for i in range(n):
      for j in range(n):
        print(solver.Value(x[(i, j)]), end=" ")
      print()
    print()

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  # data
  n = 31  # the grid size
  c = 14  # number of coins per row/column
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  if len(sys.argv) > 2:
    c = int(sys.argv[2])

  main(n, c)
