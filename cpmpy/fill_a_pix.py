"""
Fill-a-Pix problem in cpmpy.

From
http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
'''
Each puzzle consists of a grid containing clues in various places. The
object is to reveal a hidden picture by painting the squares around each
clue so that the number of painted squares, including the square with
the clue, matches the value of the clue.
'''

http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
'''
Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated
picture hidden inside. Using logic alone, the solver determines which
squares are painted and which should remain empty until the hidden picture
is completely exposed.
'''

Fill-a-pix History:
http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


# Puzzle 1 from
# http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
default_n = 10
X = -1
default_puzzle = [
    [X, X, X, X, X, X, X, X, 0, X], [X, 8, 8, X, 2, X, 0, X, X, X],
    [5, X, 8, X, X, X, X, X, X, X], [X, X, X, X, X, 2, X, X, X, 2],
    [1, X, X, X, 4, 5, 6, X, X, X], [X, 0, X, X, X, 7, 9, X, X, 6],
    [X, X, X, 6, X, X, 9, X, X, 6], [X, X, 6, 6, 8, 7, 8, 7, X, 5],
    [X, 4, X, 6, 6, 6, X, 6, X, 4], [X, X, X, X, X, X, 3, X, X, X]
]


def fill_a_pix(puzzle='', n=''):

  model = Model()
  
  # data

  # Set default problem
  if puzzle == '':
    puzzle = default_puzzle
    n = default_n
  else:
    print('n:', n)

  # for the neighbors of 'this' cell
  S = [-1, 0, 1]

  # print problem instance
  print('Problem:')
  for i in range(n):
    for j in range(n):
      if puzzle[i][j] == X:
        sys.stdout.write('.')
      else:
        sys.stdout.write(str(puzzle[i][j]))
    print()
  print()

  # declare variables
  pict = boolvar(shape=(n,n),name="pict")

  #
  # constraints
  #
  for i in range(n):
    for j in range(n):
      if puzzle[i][j] > X:
        # this cell is the sum of all the surrounding cells
        model += [puzzle[i][j] == sum([pict[i + a, j + b]
                                       for a in S
                                       for b in S
                                       if i + a >= 0 and
                                       j + b >= 0 and
                                       i + a < n and
                                       j + b < n
        ])]

  def print_sol():
    for i in range(n):
      row = [str(pict[i, j].value()) for j in range(n)]
      for j in range(n):
        if row[j] == '0':
          row[j] = ' '
        else:
          row[j] = '#'
      print(''.join(row))
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print('num_solutions:', num_solutions)


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, 'r')
  n = int(f.readline())
  puzzle = []
  for i in range(n):
    x = f.readline()
    row = [0] * n
    for j in range(n):
      if x[j] == '.':
        tmp = -1
      else:
        tmp = int(x[j])
      row[j] = tmp
    puzzle.append(row)
  return [puzzle, n]


if len(sys.argv) > 1:
  file = sys.argv[1]
  print('Problem instance from', file)
  [puzzle, n] = read_problem(file)
  fill_a_pix(puzzle, n)
else:
  fill_a_pix()
