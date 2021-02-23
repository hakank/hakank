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

  Minesweeper in OR-tools CP-SAT Solver.

  From gecode/examples/minesweeper.cc:
  '''
  A specification is a square matrix of characters. Alphanumeric
  characters represent the number of mines adjacent to that field.
  Dots represent fields with an unknown number of mines adjacent to
  it (or an actual mine).
  '''

  E.g.
       '..2.3.'
       '2.....'
       '..24.3'
       '1.34..'
       '.....3'
       '.3.3..'


  Also see:
  * http://www.janko.at/Raetsel/Minesweeper/index.htm

  * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)

  * Ian Stewart on Minesweeper:
    http://www.claymath.org/Popular_Lectures/Minesweeper/

  * Richard Kaye's Minesweeper Pages
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm

  * Some Minesweeper Configurations
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf


  This is a port of my old CP model minesweeper.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import *


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, mines, rows, cols):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__mines = mines 
        self.__rows = rows
        self.__cols = cols
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        r = self.__rows 
        c = self.__cols
        minesval = [self.Value(self.__mines[(i,j)]) 
                    for i in range(r) for j in range(c)  ]
        for i in range(r):
          for j in range(c):
            print(minesval[i * c + j], end=" ")
          print()
        print()


    def SolutionCount(self):
        return self.__solution_count


default_r = 8
default_c = 8
X = -1
default_game = [[2, 3, X, 2, 2, X, 2, 1], [X, X, 4, X, X, 4, X, 2],
                [X, X, X, X, X, X, 4, X], [X, 5, X, 6, X, X, X, 2],
                [2, X, X, X, 5, 5, X, 2], [1, 3, 4, X, X, X, 4, X],
                [0, 1, X, 4, X, X, X, 3], [0, 1, 2, X, 2, 3, X, 2]]


def main(game="", r="", c=""):

  model = cp.CpModel()

  #
  # data
  #

  # Set default problem
  if game == "":
    game = default_game
    r = default_r
    c = default_c
  else:
    print("rows:", r, " cols:", c)

  #
  # Default problem from "Some Minesweeper Configurations",page 3
  # (same as problem instance minesweeper_config3.txt)
  # It has 4 solutions
  #
  # r = 8
  # c = 8
  # X = -1
  # game = [
  #     [2,3,X,2,2,X,2,1],
  #     [X,X,4,X,X,4,X,2],
  #     [X,X,X,X,X,X,4,X],
  #     [X,5,X,6,X,X,X,2],
  #     [2,X,X,X,5,5,X,2],
  #     [1,3,4,X,X,X,4,X],
  #     [0,1,X,4,X,X,X,3],
  #     [0,1,2,X,2,3,X,2]
  #     ]

  S = [-1, 0, 1]  # for the neighbors of "this" cell

  # print problem instance
  print("Problem:")
  for i in range(r):
    for j in range(c):
      if game[i][j] == X:
        print("X", end=" ")
      else:
        print(game[i][j], end=" ")
    print()
  print()

  # declare variables
  mines = {}
  for i in range(r):
    for j in range(c):
      mines[(i, j)] = model.NewIntVar(0, 1, "mines %i %i" % (i, j))

  #
  # constraints
  #
  for i in range(r):
    for j in range(c):
      if game[i][j] >= 0:
        model.Add(mines[i, j] == 0)
        # this cell is the sum of all the surrounding cells
        model.Add(game[i][j] == sum([
            mines[i + a, j + b]
            for a in S
            for b in S
            if i + a >= 0 and j + b >= 0 and i + a < r and j + b < c
        ]))
      if game[i][j] > X:
        # This cell cannot be a mine
        model.Add(mines[i, j] == 0)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  solution_printer = SolutionPrinter(mines,r,c) 
  status = solver.SearchForAllSolutions(model,solution_printer)

  if status == cp.OPTIMAL:
    print()
    print("NumSolutions:", solution_printer.SolutionCount())
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())

  else:
    print("No solutions found")


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  rows = int(f.readline())
  cols = int(f.readline())
  game = []
  for i in range(rows):
    x = f.readline()
    row = [0] * cols
    for j in range(cols):
      if x[j] == ".":
        tmp = -1
      else:
        tmp = int(x[j])
      row[j] = tmp
    game.append(row)
  return [game, rows, cols]


#
# Print the mines
#
def print_mines(mines, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print(mines[i, j], end=" ")
    print("")


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print(game[i][j], end=" ")
    print("")


if __name__ == "__main__":
  if len(sys.argv) > 1:
    file = sys.argv[1]
    print("Problem instance from", file)
    [game, rows, cols] = read_problem(file)
    # print_game(game, rows, cols)
    main(game, rows, cols)
  else:
    main()
