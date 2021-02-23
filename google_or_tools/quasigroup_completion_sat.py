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
  Quasigroup completion OR-tools CP-SAT Solver.

  See Carla P. Gomes and David Shmoys:
  "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"

  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  '''
    Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers
    into
    a four-by-four array so that no column or row contains the same two numbers.
    The result is known as a Latin square.
    ...
    The so-called quasigroup completion problem concerns a table that is
    correctly
    but only partially filled in. The question is whether the remaining blanks
    in
    the table can be filled in to obtain a complete Latin square (or a proper
    quasigroup multiplication table).
    '''

  This is a port of my old CP model quasigroup_completion.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n, x):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__x = x 
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print(f"\nSolution #{self.__solution_count}")
        n = self.__n
        # xval = [self.Value(self.__x[(i, j)]) for i in range(n) for j in range(n)]
        x = [[self.Value(self.__x[(i, j)]) for i in range(n)] for j in range(n)]
        print_game(x, n, n)

    def SolutionCount(self):
        return self.__solution_count


default_n = 5
X = 0
# default problem
# (This is the same as quasigroup1.txt)
default_puzzle = [[1, X, X, X, 4], [X, 5, X, X, X], [4, X, X, 2, X],
                  [X, 4, X, X, X], [X, X, 5, X, 1]]


def main(puzzle="", n=0):

  model = cp.CpModel()

  #
  # data
  #
  if puzzle == "":
    puzzle = default_puzzle
    n = default_n

  print("Problem:")
  print_game(puzzle, n, n)

  # declare variables
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = model.NewIntVar(1, n, "x %i %i" % (i, j))

  #
  # constraints
  #

  #
  # set the clues
  #
  for i in range(n):
    for j in range(n):
      if puzzle[i][j] > X:
        model.Add(x[i, j] == puzzle[i][j])

  #
  # rows and columns must be different
  #
  for i in range(n):
    model.AddAllDifferent([x[i, j] for j in range(n)])
    model.AddAllDifferent([x[j, i] for j in range(n)])

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(n, x)
  status = solver.SearchForAllSolutions(model,solution_printer)

  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("\nNo solution!")

  # if num_solutions == 0:
  #    print("No solutions found")

  print()
  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  n = int(f.readline())
  game = []
  for i in range(n):
    x = f.readline()
    row_x = (x.rstrip()).split(" ")
    row = [0] * n
    for j in range(n):
      if row_x[j] == ".":
        tmp = 0
      else:
        tmp = int(row_x[j])
      row[j] = tmp
    game.append(row)
  return [game, n]


def print_board(x, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 2s" % x[i, j], end=" ")
    print("")


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 2s" % game[i][j], end=" ")
    print("")


if __name__ == "__main__":
  if len(sys.argv) > 1:
    file = sys.argv[1]
    print("Problem instance from", file)
    [game, n] = read_problem(file)
    main(game, n)
  else:
    main()
