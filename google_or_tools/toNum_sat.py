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

  toNum in OR-tools CP-SAT Solver.

  Convert a number <-> array of int in a specific base.

  This is a port of my old CP model toNum.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import toNum, SimpleSolutionPrinter, ListPrinter

# #
# # converts a number (s) <-> an array of integers (t) in the specific base.
# #
# def toNum(solver, t, s, base):
#   tlen = len(t)
#   solver.Add(
#       s == solver.Sum([(base**(tlen - i - 1)) * t[i] for i in range(tlen)]))


def main():

  model = cp.CpModel()

  # data
  n = 4
  base = 10

  # declare variables
  x = [model.NewIntVar(0, n - 1, "x%i" % i) for i in range(n)]
  y = model.NewIntVar(0, 10**n - 1, "y")

  #
  # constraints
  #
  # solver.Add(solver.AllDifferent([x[i] for i in range(n)]))
  model.AddAllDifferent(x)
  # solver.Add(x[0] > 0) # just for fun

  toNum(model, x, y, base)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  all = x 
  all.append(y)
  # solution_printer = SimpleSolutionPrinter(all)
  solution_printer = ListPrinter(all)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if status != cp.OPTIMAL:
    print("No solution!")

  print()
  print("NumSolutions", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
