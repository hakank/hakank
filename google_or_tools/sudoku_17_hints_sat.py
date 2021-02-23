# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
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

  Sudoku 17 hints in OR-tools CP-SAT Solver.

  This model checks all 49151 Sudoku instances with 17 hints from
  Gordon Royle:
  http://staffhome.ecm.uwa.edu.au/~0001e3890/sudoku17

  Also see http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php

  The Sudoku solver is based on sudoku.py from the OR-tools CP distribution
  and is somewhat alterered.

  This is a port of my old CP model sudoku_17_hints.py

  Solving all the 49151 problems took 88.618s (wall time)
    i.e. about 49151/88.618 ~ 555 problems/s.

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or-tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *
import re


def main(initial_grid,print_sol=1):

  model = cp.CpModel()  

  cell_size = 3
  line_size = cell_size ** 2
  line = range(0, line_size)
  cell = range(0, cell_size)

  grid = {}
  for i in line:
    for j in line:
      grid[(i, j)] = model.NewIntVar(1, line_size, 'grid %i %i' % (i, j))

  # AllDifferent on rows.
  for i in line:
    model.AddAllDifferent([grid[(i, j)] for j in line])

  # AllDifferent on columns.
  for j in line:
    model.AddAllDifferent([grid[(i, j)] for i in line])

  # AllDifferent on cells.
  for i in cell:
    for j in cell:
      one_cell = []
      for di in cell:
        for dj in cell:
          one_cell.append(grid[(i * cell_size + di, j * cell_size + dj)])

      model.AddAllDifferent(one_cell)

  # Initial values.
  for i in line:
    for j in line:
      if initial_grid[i][j]:
        model.Add(grid[(i, j)] == initial_grid[i][j])

  # Regroup all variables into a list.

  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    True
    # if(print_sol):
    #  for i in line:
    #    print([solver.Value(grid[(i, j)]) for j in line])
    
    

  # if print_sol:
  #  print
  #  print "num_solutions: ", num_solutions
  #  print "failures:", solver.Failures()
  #  print "branches:", solver.Branches()
  #  print "WallTime:", solver.WallTime()

  return solver.WallTime()

def slice_sudoku(sudoku):
  return [ sudoku[i*9:i*9+9] for i in range(9)]


if __name__ == '__main__':
  # initial_grid = [[0, 6, 0, 0, 5, 0, 0, 2, 0],
  #                 [0, 0, 0, 3, 0, 0, 0, 9, 0],
  #                 [7, 0, 0, 6, 0, 0, 0, 1, 0],
  #                 [0, 0, 6, 0, 3, 0, 4, 0, 0],
  #                 [0, 0, 4, 0, 7, 0, 1, 0, 0],
  #                 [0, 0, 5, 0, 9, 0, 8, 0, 0],
  #                 [0, 4, 0, 0, 0, 1, 0, 0, 6],
  #                 [0, 3, 0, 0, 0, 8, 0, 0, 0],
  #                 [0, 2, 0, 0, 4, 0, 0, 5, 0]]
  # main(initial_grid)
  
  sudokus = [slice_sudoku([int(c) for c in sudoku.strip()]) for sudoku in open("sudoku17").readlines()]
  # wall_time = main(sudokus[0],0)
  # print wall_time
  all_wall_time = 0
  n = 1
  for sudoku in sudokus:
    wall_time = main(sudoku,0)
    print("%", n, ":", wall_time)
    all_wall_time += wall_time
    n += 1

  print("all_wall_time:", all_wall_time)
  
    
  
  


