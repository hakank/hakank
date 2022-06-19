"""
Killer Sudoku in Google CP Solver.

http://en.wikipedia.org/wiki/Killer_Sudoku
'''
Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or
samunamupure) is a puzzle that combines elements of sudoku and kakuro.
Despite the name, the simpler killer sudokus can be easier to solve
than regular sudokus, depending on the solver's skill at mental arithmetic;
the hardest ones, however, can take hours to crack.

...

The objective is to fill the grid with numbers from 1 to 9 in a way that
the following conditions are met:

  * Each row, column, and nonet contains each number exactly once.
  * The sum of all numbers in a cage must match the small number printed
    in its corner.
  * No number appears more than once in a cage. (This is the standard rule
    for killer sudokus, and implies that no cage can include more
    than 9 cells.)

In 'Killer X', an additional rule is that each of the long diagonals
contains each number once.
'''

Here we solve the problem from the Wikipedia page, also shown here
http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

The output is:
  2 1 5 6 4 7 3 9 8
  3 6 8 9 5 2 1 7 4
  7 9 4 3 8 1 6 5 2
  5 8 6 2 7 4 9 3 1
  1 4 2 5 9 3 8 6 7
  9 7 3 8 1 6 4 2 5
  8 2 1 7 3 9 5 4 6
  6 5 9 4 2 8 7 1 3
  4 3 7 1 6 5 2 8 9


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


#
# Ensure that the sum of the segments
# in cc == res
#
def calc(cc, x, res):
  constraints = []
  
  # sum the numbers
  cage = [x[i[0] - 1, i[1] - 1] for i in cc]
  constraints += [sum(cage) == res]
  constraints += [AllDifferent(cage)]
  return constraints

def killer_sudoku():

  model = Model()
  
  # data

  # size of matrix
  n = 9

  # For a better view of the problem, see
  #  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

  # hints
  #    [sum, [segments]]
  # Note: 1-based
  problem = [[3, [[1, 1], [1, 2]]], [15, [[1, 3], [1, 4], [1, 5]]],
             [22, [[1, 6], [2, 5], [2, 6], [3, 5]]], [4, [[1, 7], [2, 7]]],
             [16, [[1, 8], [2, 8]]], [15, [[1, 9], [2, 9], [3, 9], [4, 9]]],
             [25, [[2, 1], [2, 2], [3, 1], [3, 2]]], [17, [[2, 3], [2, 4]]],
             [9, [[3, 3], [3, 4], [4, 4]]], [8, [[3, 6], [4, 6], [5, 6]]],
             [20, [[3, 7], [3, 8], [4, 7]]], [6, [[4, 1], [5, 1]]],
             [14, [[4, 2], [4, 3]]], [17, [[4, 5], [5, 5], [6, 5]]],
             [17, [[4, 8], [5, 7], [5, 8]]], [13, [[5, 2], [5, 3], [6, 2]]],
             [20, [[5, 4], [6, 4], [7, 4]]], [12, [[5, 9], [6, 9]]],
             [27, [[6, 1], [7, 1], [8, 1], [9, 1]]],
             [6, [[6, 3], [7, 2], [7, 3]]], [20, [[6, 6], [7, 6], [7, 7]]],
             [6, [[6, 7], [6, 8]]], [10, [[7, 5], [8, 4], [8, 5], [9, 4]]],
             [14, [[7, 8], [7, 9], [8, 8], [8, 9]]], [8, [[8, 2], [9, 2]]],
             [16, [[8, 3], [9, 3]]], [15, [[8, 6], [8, 7]]],
             [13, [[9, 5], [9, 6], [9, 7]]], [17, [[9, 8], [9, 9]]]]

  # variables
  x = intvar(1,n,shape=(n,n),name="x")

  # constraints

  # all rows and columns must be unique
  model += [ AllDifferent(row) for row in x]
  model += [ AllDifferent(col) for col in x.transpose()]  

  # cells
  for i in range(2):
    for j in range(2):
      cell = [
          x[r, c]
          for r in range(i * 3, i * 3 + 3)
          for c in range(j * 3, j * 3 + 3)
      ]
      model += [AllDifferent(cell)]

  # calculate the segments
  for (res, segment) in problem:
    model += [calc(segment, x, res)]

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:", num_solutions)


killer_sudoku()
