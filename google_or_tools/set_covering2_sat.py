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

  Set covering in OR-tools CP-SAT Solver.

  Example 9.1-2, page 354ff, from
  Taha 'Operations Research - An Introduction'
  Minimize the number of security telephones in street
  corners on a campus.

  This is port of my old CP model set_covering2.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
# import math, sys
# from cp_sat_utils import *


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 8  # maximum number of corners
  num_streets = 11  # number of connected streets

  # corners of each street
  # Note: 1-based (handled below)
  corner = [[1, 2], [2, 3], [4, 5], [7, 8], [6, 7], [2, 6], [1, 6], [4, 7],
            [2, 4], [5, 8], [3, 5]]

  #
  # declare variables
  #
  x = [model.NewIntVar(0, 1, "x[%i]" % i) for i in range(n)]
  z = model.NewIntVar(0, n, "z")

  #
  # constraints
  #

  # number of telephones, to be minimized
  model.Add(z == sum(x))

  # ensure that all corners are covered
  for i in range(num_streets):
    # also, convert to 0-based
    model.Add(sum([x[j - 1] for j in corner[i]]) >= 1)

  model.Minimize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("z:", solver.Value(z))
    print("x:", [solver.Value(x[i]) for i in range(n)])

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
