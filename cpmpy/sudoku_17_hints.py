"""
Sudoku 17 hints in OR-tools CP-SAT Solver.

This model checks all 49151 Sudoku instances with 17 hints from
Gordon Royle:
http://staffhome.ecm.uwa.edu.au/~0001e3890/sudoku17

Also see http://staffhome.ecm.uwa.edu.au/~00013890/sudokumin.php

Solving all the 49151 problems took 94.48s (Total run time: 276.35s)
    i.e. about 49151/94.48 ~ 520p roblems/s.

This CPMpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my CPMpy page: http://hakank.org/cpmpy/

"""
import math, sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import CPM_ortools
from cpmpy_hakank import *
import sys


def slice_sudoku(sudoku):
  return [ sudoku[i*9:i*9+9] for i in range(9)]

def sudoku_17_hints(initial_grid,print_sol=1):

  model = Model()

  cell_size = 3
  line_size = cell_size ** 2
  line = range(0, line_size)
  cell = range(0, cell_size)

  grid = intvar(1,line_size,shape=(line_size,line_size),name="grid")

  # Initial values.
  for i in line:
    for j in line:
      if initial_grid[i][j]:
        model += [grid[i, j] == initial_grid[i][j]]

  # AllDifferent on rows.
  for i in line:
    model += [AllDifferent([grid[i, j] for j in line])]

  # AllDifferent on columns.
  for j in line:
    model += [AllDifferent([grid[i, j] for i in line])]

  # AllDifferent on cells.
  for i in cell:
    for j in cell:
      one_cell = []
      for di in cell:
        for dj in cell:
          one_cell.append(grid[i * cell_size + di, j * cell_size + dj])

      model += [AllDifferent(one_cell)]
 
  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.log_search_progress = True
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0

  if ss.solve():
    if(print_sol):
        print(grid.value())
    return ss.ort_solver.WallTime()
  else:
    print("No solution found")
    return 99999


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

sudokus = [slice_sudoku([int(c) for c in sudoku.strip()]) for sudoku in open("sudoku17.txt").readlines()]
# wall_time = main(sudokus[0],0)
# print wall_time
all_wall_time = 0
n = 1
print_sol = 0
for sudoku in sudokus:
  wall_time = sudoku_17_hints(sudoku,print_sol)
  print("%", n, ":", wall_time)
  all_wall_time += wall_time
  n += 1

print("all_wall_time:", all_wall_time)
print("It was ", n-1, "instances")
