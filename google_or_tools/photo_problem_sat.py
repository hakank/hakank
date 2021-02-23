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
  Photo problem in OR-tools CP-SAT Solver.

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  '''
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one
  row for taking a photo. Some of them have preferences next to whom
  they want to stand:

     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.

  Obviously, it is impossible to satisfy all preferences. Can you find
  an alignment that maximizes the number of satisfied preferences?
  '''

  Oz solution:
    6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]

  This is a port of my old CP model photo_problem.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import flatten, ListPrinter


def main(opt = 0):

  model = cp.CpModel()

  #
  # data
  #
  persons = ["Betty", "Chris", "Donald", "Fred", "Gary", "Mary", "Paul"]
  n = len(persons)
  preferences = [
      # 0 1  2  3  4  5  6
      # B C  D  F  G  M  P
      [0, 0, 0, 0, 1, 1, 0],  # Betty  0
      [1, 0, 0, 0, 1, 0, 0],  # Chris  1
      [0, 0, 0, 0, 0, 0, 0],  # Donald 2
      [0, 0, 1, 0, 0, 1, 0],  # Fred   3
      [0, 0, 0, 0, 0, 0, 0],  # Gary   4
      [0, 0, 0, 0, 0, 0, 0],  # Mary   5
      [0, 0, 1, 1, 0, 0, 0]   # Paul   6
  ]

  if opt == 0:
    print("""Preferences:
      1. Betty wants to stand next to Gary and Mary.
      2. Chris wants to stand next to Betty and Gary.
      3. Fred wants to stand next to Mary and Donald.
      4. Paul wants to stand next to Fred and Donald.
      """)

  #
  # declare variables
  #
  positions = [model.NewIntVar(0, n - 1, "positions[%i]" % i) for i in range(n)]

  # successful preferences
  # z = model.NewIntVar(0, n * n, "z")
  z = model.NewIntVar(0, sum(flatten(preferences)), "z")

  #
  # constraints
  #
  model.AddAllDifferent(positions)

  # calculate all the successful preferences
  
  # b = [model.NewBoolVar("") for i in range(n) for j in range(n)]
  bb = []
  for i in range(n):
    for j in range(n):
      if preferences[i][j] == 1:
        b = model.NewBoolVar("b")
        p = model.NewIntVar(-n,n,"p")
        model.Add(p == positions[i] - positions[j])
        d = model.NewIntVar(-n,n, "d")
        # This don't work:
        # model.AddAbsEquality(d,p).OnlyEnforce(b)
        model.AddAbsEquality(d,p)
        model.Add(d == 1).OnlyEnforceIf(b)

        bb.append(b)

  model.Add(z == sum(bb))

  #
  # Symmetry breaking (from the Oz page):
  #   Fred is somewhere left of Betty
  model.Add(positions[3] < positions[0])

  # objective
  if opt == 0:
    model.Maximize(z)
  else:
    model.Add(z == opt)

  #
  # search and result
  #
  solver = cp.CpSolver()
  if opt == 0:
    status = solver.Solve(model)
  else:
    solution_printer = ListPrinter(positions)
    status = solver.SearchForAllSolutions(model,solution_printer)

  print("status:", solver.StatusName(status))
  if opt == 0 and (status == cp.OPTIMAL or status == cp.FEASIBLE):  
    print("z:", solver.Value(z))
    p = [solver.Value(positions[i]) for i in range(n)]
    print("p:",p)

    print(" ".join(
        [persons[j] for i in range(n) for j in range(n) if p[j] == i]))
    print("Successful preferences:")
    for i in range(n):
      for j in range(n):
        if preferences[i][j] == 1 and abs(p[i] - p[j]) == 1:
          print("\t", persons[i], persons[j])
    print()

    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())
    print()

  return solver.Value(z)

if __name__ == "__main__":
  print("Get max number of matchings")
  z = main(opt = 0)
  print("Get all optimal solutions with z =", z)
  main(z)
