#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# A programming puzzle from Einav in Z3
#
# From
# 'A programming puzzle from Einav'
# http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
# '''
# My friend Einav gave me this programming puzzle to work on. Given
# this array of positive and negative numbers:
# 33   30  -10 -6  18   7  -11 -23   6
# ...
# -25   4  16  30  33 -23  -4   4 -23
#
# You can flip the sign of entire rows and columns, as many of them
# as you like. The goal is to make all the rows and columns sum to positive
# numbers (or zero), and then to find the solution (there are more than one)
# that has the smallest overall sum. So for example, for this array:
# 33  30 -10
# -16  19   9
# -17 -12 -14
# You could flip the sign for the bottom row to get this array:
# 33  30 -10
# -16  19   9
# 17  12  14
# Now all the rows and columns have positive sums, and the overall total is
# 108.
# But you could instead flip the second and third columns, and the second
# row, to get this array:
# 33  -30  10
# 16   19    9
# -17   12   14
# All the rows and columns still total positive, and the overall sum is just
# 66. So this solution is better (I don't know if it's the best)
# A pure brute force solution would have to try over 30 billion solutions.
# I wrote code to solve this in J. I'll post that separately.
# '''
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  # sol = Solver()
  sol = SolverFor("QF_NIA")

  #
  # data
  #

  # small problem
  # rows = 3;
  # cols = 3;
  # data = [
  #     [ 33,  30, -10],
  #     [-16,  19,   9],
  #     [-17, -12, -14]
  #     ]

  # Full problem
  rows = 27
  cols = 9
  data = [
      [33, 30, 10, -6, 18, -7, -11, 23, -6],
      [16, -19, 9, -26, -8, -19, -8, -21, -14],
      [17, 12, -14, 31, -30, 13, -13, 19, 16],
      [-6, -11, 1, 17, -12, -4, -7, 14, -21],
      [18, -31, 34, -22, 17, -19, 20, 24, 6],
      [33, -18, 17, -15, 31, -5, 3, 27, -3],
      [-18, -20, -18, 31, 6, 4, -2, -12, 24],
      [27, 14, 4, -29, -3, 5, -29, 8, -12],
      [-15, -7, -23, 23, -9, -8, 6, 8, -12],
      [33, -23, -19, -4, -8, -7, 11, -12, 31],
      [-20, 19, -15, -30, 11, 32, 7, 14, -5],
      [-23, 18, -32, -2, -31, -7, 8, 24, 16],
      [32, -4, -10, -14, -6, -1, 0, 23, 23],
      [25, 0, -23, 22, 12, 28, -27, 15, 4],
      [-30, -13, -16, -3, -3, -32, -3, 27, -31],
      [22, 1, 26, 4, -2, -13, 26, 17, 14],
      [-9, -18, 3, -20, -27, -32, -11, 27, 13],
      [-17, 33, -7, 19, -32, 13, -31, -2, -24],
      [-31, 27, -31, -29, 15, 2, 29, -15, 33],
      [-18, -23, 15, 28, 0, 30, -4, 12, -32],
      [-3, 34, 27, -25, -18, 26, 1, 34, 26],
      [-21, -31, -10, -13, -30, -17, -12, -26, 31],
      [23, -31, -19, 21, -17, -10, 2, -23, 23],
      [-3, 6, 0, -3, -32, 0, -10, -25, 14],
      [-19, 9, 14, -27, 20, 15, -5, -27, 18],
      [11, -6, 24, 7, -17, 26, 20, -31, -25],
      [-25, 4, -16, 30, 33, 23, -4, -4, 23]
  ]

  #
  # variables
  #
  x = {}
  for i in range(rows):
    for j in range(cols):
      x[i, j] = makeIntVar(sol, 'x[%i,%i]' % (i, j), -100, 100)

  x_flat = [x[i, j] for i in range(rows) for j in range(cols)]

  row_sums = [makeIntVar(sol, 'row_sums(%i)' % i, 0, 300)
              for i in range(rows)]
  col_sums = [makeIntVar(sol, 'col_sums(%i)' % j, 0, 300)
              for j in range(cols)]

  row_signs = [makeIntVarVals(sol, 'row_signs(%i)' % i,[-1, 1])
               for i in range(rows)]
  col_signs = [makeIntVarVals(sol, 'col_signs(%i)' % j, [-1, 1])
               for j in range(cols)]

  # total sum: to be minimized
  total_sum = makeIntVar(sol, 'total_sum', 0, 1000)

  #
  # constraints
  #
  for i in range(rows):
    for j in range(cols):
      sol.add(x[i, j] == data[i][j] * row_signs[i] * col_signs[j])

  sol.add(total_sum == Sum([data[i][j] * row_signs[i] * col_signs[j]
                            for i in range(rows) for j in range(cols)]))

  # row sums
  for i in range(rows):
    sol.add(row_sums[i] == Sum([row_signs[i] * col_signs[j] * data[i][j]
                                for j in range(cols)]))

  # column sums
  for j in range(cols):
    sol.add(col_sums[j] == Sum([row_signs[i] * col_signs[j] * data[i][j]
                                for i in range(rows)]))

  # objective
  # sol.minimize(total_sum)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('total_sum:', mod.eval(total_sum))
    print('row_sums:', [mod.eval(row_sums[i]) for i in range(rows)])
    print('col_sums:', [mod.eval(col_sums[j]) for j in range(cols)])
    print('row_signs:', [mod.eval(row_signs[i]) for i in range(rows)])
    print('col_signs:', [mod.eval(col_signs[j]) for j in range(cols)])
    print('x:')
    for i in range(rows):
      for j in range(cols):
        print('%3i' % mod.eval(x[i, j]).as_long(), end=' ')
      print()
    print()
    getLessSolution(sol,mod,total_sum)

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
