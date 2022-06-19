"""
Futoshiki problem in cpmpy.

From http://en.wikipedia.org/wiki/Futoshiki
'''
The puzzle is played on a square grid, such as 5 x 5. The objective
is to place the numbers 1 to 5 (or whatever the dimensions are)
such that each row, and column contains each of the digits 1 to 5.
Some digits may be given at the start. In addition, inequality
constraints are also initially specifed between some of the squares,
such that one must be higher or lower than its neighbour. These
constraints must be honoured as the grid is filled out.
'''

Also see
http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley

This is inspired by the Minion/Tailor example futoshiki.eprime.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def futoshiki(values, lt):

  model = Model()
  #
  # data
  #
  size = len(values)
  RANGE = list(range(size))
  NUMQD = list(range(len(lt)))

  # variables
  field = intvar(1,size,shape=(size,size),name="field")


  # constraints

  # set initial values
  for row in RANGE:
    for col in RANGE:
      if values[row][col] > 0:
        model += [field[row, col] == values[row][col]]

  # all rows have to be different
  for row in RANGE:
    model += [AllDifferent([field[row, col] for col in RANGE])]

  # all columns have to be different
  for col in RANGE:
    model += [AllDifferent([field[row, col] for row in RANGE])]

  # all < constraints are satisfied
  # Also: make 0-based
  for i in NUMQD:
    model += [
        field[lt[i][0]-1, lt[i][1]-1] < field[lt[i][2]-1, lt[i][3]-1]]

  def print_sol():
    for i in RANGE:
      for j in RANGE:
        print(field[i, j].value(), end=" ")
      print()
    print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


#
# Example from Tailor model futoshiki.param/futoshiki.param
# Solution:
# 5 1 3 2 4
# 1 4 2 5 3
# 2 3 1 4 5
# 3 5 4 1 2
# 4 2 5 3 1
#
# Futoshiki instance, by Andras Salamon
# specify the numbers in the grid
#
values1 = [[0, 0, 3, 2, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0],
           [0, 0, 0, 0, 0]]

# [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
# Note: 1-based
lt1 = [[1, 2, 1, 1], [1, 4, 1, 5], [2, 3, 1, 3], [3, 3, 2, 3], [3, 4, 2, 4],
       [2, 5, 3, 5], [3, 2, 4, 2], [4, 4, 4, 3], [5, 2, 5, 1], [5, 4, 5, 3],
       [5, 5, 4, 5]]

#
# Example from http://en.wikipedia.org/wiki/Futoshiki
# Solution:
# 5 4 3 2 1
# 4 3 1 5 2
# 2 1 4 3 5
# 3 5 2 1 4
# 1 2 5 4 3
#
values2 = [[0, 0, 0, 0, 0], [4, 0, 0, 0, 2], [0, 0, 4, 0, 0], [0, 0, 0, 0, 4],
           [0, 0, 0, 0, 0]]

# Note: 1-based
lt2 = [[1, 2, 1, 1], [1, 4, 1, 3], [1, 5, 1, 4], [4, 4, 4, 5], [5, 1, 5, 2],
       [5, 2, 5, 3]]

print("Problem 1")
futoshiki(values1, lt1)
print("\nProblem 2")
futoshiki(values2, lt2)
