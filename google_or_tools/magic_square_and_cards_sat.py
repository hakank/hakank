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

  Magic squares and cards problem in Google CP-SAT Solver.

  Martin Gardner (July 1971)
  '''
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  '''

  This is a port of my old CP model magic_square_and_cards.py

  Here are the solutions for n=3

    n: 3      
    s: 36
    counts: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3]
    12  13  11 
    11  12  13 
    13  11  12 

    NumConflicts: 6
    NumBranches: 447
    WallTime: 0.018104907

  And for n=4..7 (n=7 is the largest possible)
    n: 4
    s: 46
    counts: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 4, 4]
    11  12  12  11 
    13  10  10  13 
    10  13  13  10 
    12  11  11  12 

    n: 5
    NumConflicts: 37
    NumBranches: 832
    WallTime: 0.08976392200000001

    n: 6
    s: 51
    counts: [0, 0, 0, 1, 0, 0, 0, 0, 4, 4, 4, 4, 4, 4]
    13   9   8  12   9 
     3  10  13  13  12 
    11  13   8   8  11 
    12   9  11  10   9 
    12  10  11   8  10 

    NumConflicts: 9
    NumBranches: 1189
    WallTime: 0.036612643

    n: 7
    s: 52
    counts: [0, 0, 1, 3, 0, 2, 3, 3, 4, 4, 4, 4, 4, 4]
      9  11  10   2  11   9 
      5   5  13  11   8  10 
      3  10  13   8  10   8 
     13  11   6   7   8   7 
     13  12   3  12   6   6 
      9   3   7  12   9  12 

    NumConflicts: 2
    NumBranches: 1717
    WallTime: 0.050881432000000004

    s: 51
    counts: [0, 4, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
      3   9   8  10   5   4  12 
      6   1   9   9  12   7   7 
     10   8  10  11   5   6   1 
     13  11  11   4   3   5   4 
     10   9  11   7   8   4   2 
      3   6   1   2  13  13  13 
      6   7   1   8   5  12  12 

    NumConflicts: 186
    NumBranches: 3337
    WallTime: 1.2652944350000002


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import global_cardinality


def main(n=3):

  model = cp.CpModel()

  #
  # declare variables
  #
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = model.NewIntVar(1, 13, "x(%i,%i)" % (i, j))
  x_flat = [x[(i, j)] for i in range(n) for j in range(n)]

  s = model.NewIntVar(1, 13 * 4, "s")
  counts = [model.NewIntVar(0, 4, "counts(%i)" % i) for i in range(14)]

  #
  # constraints
  #
  global_cardinality(model, x_flat, list(range(14)), counts)

  # the standard magic square constraints (sans all_different)
  [model.Add(sum([x[(i, j)] for j in range(n)]) == s) for i in range(n)]
  [model.Add(sum([x[(i, j)] for i in range(n)]) == s) for j in range(n)]

  model.Add(sum([x[(i, i)] for i in range(n)]) == s)          # diag 1
  model.Add(sum([x[(i, n - i - 1)] for i in range(n)]) == s)  # diag 2

  # redundant constraint
  model.Add(sum(counts) == n * n)

  # objective
  model.Maximize(s)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("s:", solver.Value(s))
    print("counts:", [solver.Value(counts[i]) for i in range(14)])
    for i in range(n):
      for j in range(n):
        print("%3d" % solver.Value(x[(i, j)]), end=" ")
      print()

    print()

  print()
  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


n = 3
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  main(n)
