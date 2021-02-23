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

  Discrete tomography in OR-tools CP-SAT Solver.

  Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
  '''
  This is a little 'tomography' problem, taken from an old issue
  of Scientific American.

  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:

  ?- go.
    0 0 7 1 6 3 4 5 2 7 0 0
 0
 0
 8      * * * * * * * *
 2      *             *
 6      *   * * * *   *
 4      *   *     *   *
 5      *   *   * *   *
 3      *   *         *
 7      *   * * * * * *
 0
 0

 Eclipse solution by Joachim Schimpf, IC-Parc
 '''

  This is a port of my old CP model discrete_tomography.py
 
  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """
    SimpleSolutionPrinter: Print solution in one line.

    Example:
        # model = ...
        solution_printer = SimpleSolutionPrinter(variables)
        status = solver.SearchForAllSolutions(model, solution_printer)
        # ...
        print()
        print('Solutions found : %i' % solution_printer.SolutionCount())
        # ...
    """
    def __init__(self, x, rows, cols, row_sums, col_sums):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x
        self.__rows = rows
        self.__cols = cols
        self.__row_sums = row_sums
        self.__col_sums = col_sums
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print_solution(self, self.__x, self.__rows, self.__cols, self.__row_sums, self.__col_sums)
        print()

    def SolutionCount(self):
        return self.__solution_count



def main(row_sums="", col_sums=""):

  model = cp.CpModel()

  #
  # data
  #
  if row_sums == "":
    print("Using default problem instance")
    row_sums = [0, 0, 8, 2, 6, 4, 5, 3, 7, 0, 0]
    col_sums = [0, 0, 7, 1, 6, 3, 4, 5, 2, 7, 0, 0]

  r = len(row_sums)
  c = len(col_sums)

  # declare variables
  x = []
  for i in range(r):
    t = []
    for j in range(c):
      t.append(model.NewIntVar(0, 1, "x[%i,%i]" % (i, j)))
    x.append(t)

  #
  # constraints
  #
  [
      model.Add(sum([x[i][j]
                             for j in range(c)]) == row_sums[i])
      for i in range(r)
  ]
  [
      model.Add(sum([x[i][j]
                             for i in range(r)]) == col_sums[j])
      for j in range(c)
  ]

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  # status = solver.Solve(model)
  solution_printer = SolutionPrinter(x,r,c,row_sums, col_sums)
  status = solver.SearchForAllSolutions(model, solution_printer)

  # if status == cp.OPTIMAL:
  #   print_solution(solver, x, r, c, row_sums, col_sums)
  #   print()



  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


#
# Print solution
#
def print_solution(solver, x, rows, cols, row_sums, col_sums):
  print("  ", end=" ")
  for j in range(cols):
    print(col_sums[j], end=" ")
  print()
  for i in range(rows):
    print(row_sums[i], end=" ")
    for j in range(cols):
      if solver.Value(x[i][j]) == 1:
        print("#", end=" ")
      else:
        print(".", end=" ")
    print("")


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  row_sums = f.readline()
  col_sums = f.readline()
  row_sums = [int(r) for r in (row_sums.rstrip()).split(",")]
  col_sums = [int(c) for c in (col_sums.rstrip()).split(",")]

  return [row_sums, col_sums]


if __name__ == "__main__":
  if len(sys.argv) > 1:
    file = sys.argv[1]
    print("Problem instance from", file)
    [row_sums, col_sums] = read_problem(file)
    main(row_sums, col_sums)
  else:
    main()
