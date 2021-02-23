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

  Placing of firestations, from Winston 'Operations Research', page 486.

  This is a port of my old CP model set_covering.py

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
  min_distance = 15
  num_cities = 6

  distance = [[0, 10, 20, 30, 30, 20], [10, 0, 25, 35, 20, 10],
              [20, 25, 0, 15, 30, 20], [30, 35, 15, 0, 15, 25],
              [30, 20, 30, 15, 0, 14], [20, 10, 20, 25, 14, 0]]

  #
  # declare variables
  #
  x = [model.NewIntVar(0, 1, "x[%i]" % i) for i in range(num_cities)]
  z = model.NewIntVar(0, num_cities, "z")
  #
  # constraints
  #

  # objective to minimize
  model.Add(z == sum(x))

  # ensure that all cities are covered
  for i in range(num_cities):
    model.Add(sum([x[j] for j in range(num_cities) if distance[i][j] <= min_distance]) >= 1)

  model.Minimize(z)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("z:", solver.Value(z))
    print("x:", [solver.Value(x[i]) for i in range(num_cities)])

  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
