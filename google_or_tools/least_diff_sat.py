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

  Least diff problem in OR-tools CP-SAT Solver.

  This model solves the following problem:

  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once.

  This is a port of my old CP model least_diff.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/cp_solver/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import scalar_product


def main():

  model = cp.CpModel()

  #
  # declare variables
  #
  a = model.NewIntVar(0,9, "a")
  b = model.NewIntVar(0,9, "b")
  c = model.NewIntVar(0,9, "c")
  d = model.NewIntVar(0,9, "d")
  e = model.NewIntVar(0,9, "e")

  f = model.NewIntVar(0,9, "f")
  g = model.NewIntVar(0,9, "g")
  h = model.NewIntVar(0,9, "h")
  i = model.NewIntVar(0,9, "i")
  j = model.NewIntVar(0,9, "j")

  letters = [a, b, c, d, e, f, g, h, i, j]

  digit_vector = [10000, 1000, 100, 10, 1]
  x = model.NewIntVar(0,98765,"x")
  y = model.NewIntVar(0,98765,"y")
  # x = solver.ScalProd(letters[0:5], digit_vector)
  scalar_product(model, letters[0:5], digit_vector, x)
  # y = solver.ScalProd(letters[5:], digit_vector)
  scalar_product(model, letters[5:], digit_vector, y)
  diff = x - y

  #
  # constraints
  #
  model.Add(diff > 0)
  model.AddAllDifferent(letters)

  # objective
  model.Minimize(diff)

  #
  # solution
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    xval = solver.Value(x)
    yval = solver.Value(y)
    diffval = solver.Value(diff)
    print("x:", xval)
    print("y:", yval)
    print("diff:", diffval)
    print(xval, "-", yval, "=", diffval)
    print([("abcdefghij" [i], solver.Value(letters[i])) for i in range(10)])
    print()

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


if __name__ == "__main__":
  main()
