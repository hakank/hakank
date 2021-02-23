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
  Place number puzzle OR-tools CP-SAT Solver.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  '''
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  ""

  This is a port of my old CP model place_number_puzzle.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

def main():

  model = cp.CpModel()

  # data
  m = 32
  n = 8
  # Note: this is 1-based for compatibility (and lazyness)
  graph = [[1, 2], [1, 3], [1, 4], [2, 1], [2, 3], [2, 5], [2, 6], [3, 2],
           [3, 4], [3, 6], [3, 7], [4, 1], [4, 3], [4, 6], [4, 7], [5, 2],
           [5, 3], [5, 6], [5, 8], [6, 2], [6, 3], [6, 4], [6, 5], [6, 7],
           [6, 8], [7, 3], [7, 4], [7, 6], [7, 8], [8, 5], [8, 6], [8, 7]]

  # declare variables
  x = [model.NewIntVar(1, n, "x%i" % i) for i in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(x)
  for i in range(m):
    # Note: make 0-based
    # model.Add(abs(x[graph[i][0] - 1] - x[graph[i][1] - 1]) > 1)
    diff = model.NewIntVar(-n, n, "d")
    model.Add(diff == x[graph[i][0] - 1] - x[graph[i][1] - 1])
    diff_abs = model.NewIntVar(-n, n, "d")
    model.AddAbsEquality(diff_abs, diff)
    model.Add(diff_abs > 1)

  # symmetry breaking
  model.Add(x[0] < x[n - 1])

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  solution_printer = ListPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not (status == cp.FEASIBLE or status == cp.OPTIMAL):
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


if __name__ == "__main__":
  main()
