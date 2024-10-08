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

  Eq 10 in OR-tools CP-SAT Solver.

  Standard benchmark problem.

  This is a port of my old CP model eq10.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import *


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 7

  #
  # variables
  #
  X = [model.NewIntVar(0, 10, "X(%i)" % i) for i in range(n)]
  X1, X2, X3, X4, X5, X6, X7 = X

  #
  # constraints
  #
  model.Add(0 + 98527 * X1 + 34588 * X2 + 5872 * X3 + 59422 * X5 +
             65159 * X7 == 1547604 + 30704 * X4 + 29649 * X6)

  model.Add(0 + 98957 * X2 + 83634 * X3 + 69966 * X4 + 62038 * X5 +
             37164 * X6 + 85413 * X7 == 1823553 + 93989 * X1)

  model.Add(900032 + 10949 * X1 + 77761 * X2 + 67052 * X5 == 0 + 80197 * X3 +
             61944 * X4 + 92964 * X6 + 44550 * X7)

  model.Add(0 + 73947 * X1 + 84391 * X3 + 81310 * X5 == 1164380 + 96253 * X2 +
             44247 * X4 + 70582 * X6 + 33054 * X7)

  model.Add(0 + 13057 * X3 + 42253 * X4 + 77527 * X5 + 96552 * X7 == 1185471 +
             60152 * X1 + 21103 * X2 + 97932 * X6)

  model.Add(1394152 + 66920 * X1 + 55679 * X4 == 0 + 64234 * X2 + 65337 * X3 +
             45581 * X5 + 67707 * X6 + 98038 * X7)

  model.Add(0 + 68550 * X1 + 27886 * X2 + 31716 * X3 + 73597 * X4 +
             38835 * X7 == 279091 + 88963 * X5 + 76391 * X6)

  model.Add(0 + 76132 * X2 + 71860 * X3 + 22770 * X4 + 68211 * X5 +
             78587 * X6 == 480923 + 48224 * X1 + 82817 * X7)

  model.Add(519878 + 94198 * X2 + 87234 * X3 + 37498 * X4 == 0 + 71583 * X1 +
             25728 * X5 + 25495 * X6 + 70023 * X7)

  model.Add(361921 + 78693 * X1 + 38592 * X5 + 38478 * X6 == 0 + 94129 * X2 +
             43188 * X3 + 82528 * X4 + 69025 * X7)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    
    print("X:", [solver.Value(X[i]) for i in range(n)])
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
