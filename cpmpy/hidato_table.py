"""
Hidato puzzle using table constraint in cpmpy.

http://www.hidato.com/
'''
Puzzles start semi-filled with numbered tiles.
The first and last numbers are circled.
Connect the numbers together to win. Consecutive
number must touch horizontally, vertically, or
diagonally.
'''

This version use the table constraint and is a port
of my z3 model which is a port of Laurent Perron's
OR-tools CP model- 

Cf hidato.py which is slightly faster.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
# from cpmpy_hakank import *



def build_pairs(rows, cols):
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
              y + dy >= 0 and y + dy < cols and (dx != 0 or dy != 0))]


def Solve(model):
  """Solve the given model."""

  #
  # models, a 0 indicates an open cell which number is not yet known.
  #
  #
  puzzle = None
  if model == 1:
    # Simple problem
    puzzle = [[6, 0, 9],
              [0, 2, 8],
              [1, 0, 0]]

  elif model == 2:
    puzzle = [[0, 44, 41, 0, 0, 0, 0],
              [0, 43, 0, 28, 29, 0, 0],
              [0, 1, 0, 0, 0, 33, 0],
              [0, 2, 25, 4, 34, 0, 36],
              [49, 16, 0, 23, 0, 0, 0],
              [0, 19, 0, 0, 12, 7, 0],
              [0, 0, 0, 14, 0, 0, 0]]

  elif model == 3:
    # Problems from the book:
    # Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"
    # Problem 1 (Practice)
    puzzle = [[0, 0, 20, 0, 0],
              [0, 0, 0, 16, 18],
              [22, 0, 15, 0, 0],
              [23, 0, 1, 14, 11],
              [0, 25, 0, 0, 12]]

  elif model == 4:
    # problem 2 (Practice)
    puzzle = [[0, 0, 0, 0, 14],
              [0, 18, 12, 0, 0],
              [0, 0, 17, 4, 5],
              [0, 0, 7, 0, 0],
              [9, 8, 25, 1, 0]]

  elif model == 5:
    # problem 3 (Beginner)
    puzzle = [[0, 26, 0, 0, 0, 18],
              [0, 0, 27, 0, 0, 19],
              [31, 23, 0, 0, 14, 0],
              [0, 33, 8, 0, 15, 1],
              [0, 0, 0, 5, 0, 0],
              [35, 36, 0, 10, 0, 0]]

  elif model == 6:
    # Problem 15 (Intermediate)
    puzzle = [[64, 0, 0, 0, 0, 0, 0, 0],
              [1, 63, 0, 59, 15, 57, 53, 0],
              [0, 4, 0, 14, 0, 0, 0, 0],
              [3, 0, 11, 0, 20, 19, 0, 50],
              [0, 0, 0, 0, 22, 0, 48, 40],
              [9, 0, 0, 32, 23, 0, 0, 41],
              [27, 0, 0, 0, 36, 0, 46, 0],
              [28, 30, 0, 35, 0, 0, 0, 0]]

  elif model == 7:
      # Problem 156 (Master}
      puzzle = [
               [88, 0, 0,100, 0, 0,37,0, 0,34],
               [ 0,86, 0,96,41, 0, 0,36, 0, 0],
               [ 0,93,95,83, 0, 0, 0,31,47, 0],
               [ 0,91, 0, 0, 0, 0, 0,29, 0, 0],
               [11, 0, 0, 0, 0, 0, 0,45,51, 0],
               [ 0, 9, 5, 3, 1, 0, 0, 0, 0, 0],
               [ 0,13, 4, 0, 0, 0, 0, 0, 0, 0],
               [15, 0, 0,25, 0, 0,54,67, 0, 0],
               [ 0,17, 0,23, 0,60,59, 0,69, 0],
               [19, 0,21,62,63, 0, 0, 0, 0, 0]
               ]

  elif model == 8:
      # Problem 188 (Genius]
      puzzle = [[  0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0],
               [136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0],
               [139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0],
               [  0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103],
               [  0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101],
               [  0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0],
               [ 30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0],
               [ 32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92],
               [  0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0],
               [  0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0],
               [ 35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0],
               [ 36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88]]

  # elif model == 9:    
  #   # From NFZ
  #   # https://groups.google.com/g/picat-lang/c/899o44qEPZQ/m/5Yf9vlKLAgAJ
  #   # This instance has a lot of solutions.
  #   puzzle = [[  1,0,0,0,0, 0,0,0,0, 0],
  #             [ 20,0,0,0,0,15,0,0,0,11],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [  0,0,0,0,0, 0,0,0,0, 0],
  #             [100,0,0,0,0, 0,0,0,0, 0]]


  r = len(puzzle)
  c = len(puzzle[0])

  print(('Initial game (%i x %i)' % (r, c)))
  print_matrix(puzzle)

  #
  # declare variables
  #
  positions = intvar(0,r*c-1,shape=r*c) 

  #
  # constraints
  #
  model = Model(AllDifferent(positions))

  #
  # Fill in the clues
  #
  for i in range(r):
    for j in range(c):
      if puzzle[i][j] > 0:
        model += [(positions[puzzle[i][j] - 1] == i * c + j)]

  # Consecutive numbers much touch each other in the grid.
  # We use an allowed assignment constraint to model it.
  close_tuples = build_pairs(r, c)
  for k in range(0, r * c - 1):
    model += [Table([positions[k], positions[k + 1]],
                                         close_tuples)]

  def print_sol():
    board = []
    for unused_i in range(r):
      board.append([0] * c)
    # Fill board with solution value.
    for k in range(r * c):
      position = positions[k].value()
      board[position // c][position % c] = k + 1
    print_matrix(board)

  num_solutions = model.solveAll(display=print_sol)
  print(('solutions : %i' % num_solutions))


def print_matrix(game):
  """Pretty print of a matrix."""
  rows = len(game)
  cols = len(game[0])
  for i in range(rows):
    line = ''
    for j in range(cols):
      if game[i][j] == 0:
        line += '  .'
      else:
        line += '% 4s' % game[i][j]
    print(line)
  print()

def hidato_table():
  for model in range(1, 8+1):
    print()
    print(('----- Solving problem %i -----' % model))
    print()
    Solve(model)


hidato_table()

