#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Hidato puzzle in Z3
#  http://www.hidato.com/
# '''
# Puzzles start semi-filled with numbered tiles.
# The first and last numbers are circled.
# Connect the numbers together to win. Consecutive
# number must touch horizontally, vertically, or
# diagonally.
# '''

# This model is a port of Laurent Perron's faster version of my hidato.py
# for Google OR-tools.
#
# My slow version
# https://github.com/google/or-tools/blob/master/examples/python/hidato.py
# Laurent's faster version
# https://github.com/google/or-tools/blob/master/examples/python/hidato_table.py
#
# Also see my other Z3 models
#  - hidato.py (port of the above mentioned hidato.py or-tools model)
#  - hidato_function.py (using Function)
#
# This version use a (decomposition) of the table constraint
# (called allowed_assignment)
#
# Time to first solution:
# puzzle1 : 0.05265021324157715
# puzzle2 : 2.107395648956299
# puzzle3 : 0.4795820713043213
# puzzle4 : 0.4804689884185791
# puzzle5 : 1.0682592391967773
# puzzle6 : 3.6846492290496826
# puzzle7 : 9.88624906539917
# puzzle8 : 20.58018469810486
#
# Time to prove unicity
# puzzle1 : 0.04979300498962402
# puzzle2 : 2.098351240158081
# puzzle3 : 0.4976685047149658
# puzzle4 : 0.48920130729675293
# puzzle5 : 1.0961010456085205
# puzzle6 : 3.738976001739502
# puzzle7 : 10.018195390701294
# puzzle8 : 20.63848328590393
#
# See hidato_function.py for a comparison of the Hidato solvers.
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import time
from z3_utils_hakank import *
from hidato_instances import instances

def BuildPairs(rows, cols):
  """Build closeness pairs for consecutive numbers.

  Build set of allowed pairs such that two consecutive numbers touch
  each other in the grid.

  Returns:
    A list of pairs for allowed consecutive position of numbers.

  Args:
    rows: the number of rows in the grid
    cols: the number of columns in the grid
  """
  return [[x * cols + y, (x + dx) * cols + (y + dy)]
          for x in range(rows) for y in range(cols)
          for dx in (-1, 0, 1) for dy in (-1, 0, 1)
          if (x + dx >= 0 and x + dx < rows and
              y + dy >= 0 and y + dy < cols and
              (dx != 0 or dy != 0))]


def Solve(puzzle,num_sols=0):
  """Solve the given puzzle."""

  # sol = Solver() # Total time: 21.4s
  sol = SolverFor("QF_FD") # 7.87s
  # sol = SolverFor("QF_LIA") # 21.2s
  # sol = SolverFor("LIA") # 9.18s
  # sol = SimpleSolver() # 8.42s

  r = len(puzzle)
  c = len(puzzle[0])

  print(('Initial game (%i x %i)' % (r, c)))
  PrintMatrix(puzzle)

  #
  # declare variables
  #
  positions = [makeIntVar(sol,'p of %i' % i, 0, r * c - 1) for i in range(r * c)]

  #
  # constraints
  #
  sol.add(Distinct([positions[i] for i in range(r*c)] ))

  #
  # Fill in the clues
  #
  for i in range(r):
    for j in range(c):
      if puzzle[i][j] > 0:
        sol.add(positions[puzzle[i][j] - 1] == i * c + j)

  # Consecutive numbers much touch each other in the grid.
  # We use an allowed assignment constraint to model it.
  close_tuples = BuildPairs(r, c)
  
  for k in range(r * c - 1):
    allowed_assignments(sol, [positions[k], positions[k + 1]],
                                         close_tuples)

  #
  # solution and search
  #

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    PrintOneSolution(mod, positions, r, c, num_solutions)
    if num_sols > 0 and num_solutions >= num_sols:
      break
    sol.add(Or([ positions[i] != mod.eval(positions[i]) for i in range(r*c) ]))
    
  print(('solutions : %i' % num_solutions))


def PrintOneSolution(mod, positions, rows, cols, num_solution):
  """Print a current solution."""
  print(('Solution %i:' % num_solution))
  # Create empty board.
  board = []
  for unused_i in range(rows):
    board.append([0] * cols)
  # Fill board with solution value.
  for k in range(rows * cols):
    position = mod.eval(positions[k]).as_long()
    board[position // cols][position % cols] = k + 1
  # Print the board.
  PrintMatrix(board)


def PrintMatrix(game):
  """Pretty print of a matrix."""
  rows = len(game)
  cols = len(game[0])
  for i in range(rows):
    line = ''
    for j in range(cols):
      if game[i][j] == 0:
        line += '   .'
      else:
        line += '% 4s' % game[i][j]
    print(line)
  print()


def test_all(num_sols=0):
  times = {}
  for puzzle in instances:
    print()
    print(f"----- Solving problem {puzzle} -----")
    print()
    t0 = time.time()
    Solve(instances[puzzle],num_sols)
    t1 = time.time()
    print("Time:", t1-t0)
    times[puzzle] = t1-t0
    print()

  print("Times:")
  for puzzle in times:
    print(puzzle, ":", times[puzzle])

print("\nTime to first solution:")
num_sols = 1
test_all(num_sols)

print("Time to prove unicity:")
num_sols = 0
test_all(num_sols)
