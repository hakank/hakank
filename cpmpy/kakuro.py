"""
Kakuru puzzle in cpmpy.

http://en.wikipedia.org/wiki/Kakuro
'''
The object of the puzzle is to insert a digit from 1 to 9 inclusive
into each white cell such that the sum of the numbers in each entry
matches the clue associated with it and that no digit is duplicated in
any entry. It is that lack of duplication that makes creating Kakuro
puzzles with unique solutions possible, and which means solving a Kakuro
puzzle involves investigating combinations more, compared to Sudoku in
which the focus is on permutations. There is an unwritten rule for
making Kakuro puzzles that each clue must have at least two numbers
that add up to it. This is because including one number is mathematically
trivial when solving Kakuro puzzles; one can simply disregard the
number entirely and subtract it from the clue it indicates.
'''

This model solves the problem at the Wikipedia page.
For a larger picture, see
http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg

The solution:
  9 7 0 0 8 7 9
  8 9 0 8 9 5 7
  6 8 5 9 7 0 0
  0 6 1 0 2 6 0
  0 0 4 6 1 3 2
  8 9 3 1 0 1 4
  3 1 2 0 0 2 1


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
  # ensure that the values are positive
  for i in cc:
    constraints += [x[i[0]-1,i[1]-1] >= 1]

  # sum the numbers
  constraints += [sum([x[i[0] - 1, i[1] - 1] for i in cc]) == res]
  
  return constraints


def kakuro():

  model = Model()

  # size of matrix
  n = 7

  # segments
  #    [sum, [segments]]
  # Note: 1-based
  problem = [[16, [1, 1], [1, 2]], [24, [1, 5], [1, 6], [1, 7]],
             [17, [2, 1], [2, 2]], [29, [2, 4], [2, 5], [2, 6], [2, 7]],
             [35, [3, 1], [3, 2], [3, 3], [3, 4], [3, 5]], [7, [4, 2], [4, 3]],
             [8, [4, 5], [4, 6]], [16, [5, 3], [5, 4], [5, 5], [5, 6], [5, 7]],
             [21, [6, 1], [6, 2], [6, 3], [6, 4]], [5, [6, 6], [6, 7]],
             [6, [7, 1], [7, 2], [7, 3]], [3, [7, 6], [7, 7]],
             [23, [1, 1], [2, 1], [3, 1]], [30, [1, 2], [2, 2], [3, 2], [4, 2]],
             [27, [1, 5], [2, 5], [3, 5], [4, 5], [5, 5]], [12, [1, 6], [2, 6]],
             [16, [1, 7], [2, 7]], [17, [2, 4], [3, 4]],
             [15, [3, 3], [4, 3], [5, 3], [6, 3], [7, 3]],
             [12, [4, 6], [5, 6], [6, 6], [7, 6]], [7, [5, 4], [6, 4]],
             [7, [5, 7], [6, 7], [7, 7]], [11, [6, 1], [7, 1]],
             [10, [6, 2], [7, 2]]]

  num_p = len(problem)

  # The blanks
  # Note: 1-based
  blanks = [[1, 3], [1, 4], [2, 3], [3, 6], [3, 7], [4, 1], [4, 4], [4, 7],
            [5, 1], [5, 2], [6, 5], [7, 4], [7, 5]]
  num_blanks = len(blanks)

  #
  # variables
  #

  # the set
  x = intvar(0,9,shape=(n,n), name="x")

  #
  # constraints
  #

  # fill the blanks with 0
  for i in range(num_blanks):
    model += [x[blanks[i][0]-1, blanks[i][1]-1] == 0]

  for i in range(num_p):
    segment = problem[i][1::]
    res = problem[i][0]

    # sum this segment
    model += [calc(segment, x, res)]

    # all numbers in this segment must be distinct
    segment = [x[p[0]-1, p[1]-1] for p in segment]
    model += [AllDifferent(segment)]

  def print_sol():
    for i in range(n):
      for j in range(n):
        val = x[i, j].value()
        if val > 0:
          print(val, end=" ")
        else:
          print(" ", end=" ")
      print()
    print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


kakuro()
