#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# KenKen puzzle in Z3
# 
# http://en.wikipedia.org/wiki/KenKen
# '''
# KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
# several characteristics with sudoku. The name comes from Japanese and
# is translated as 'square wisdom' or 'cleverness squared'.
# ...
# The objective is to fill the grid in with the digits 1 through 6 such that:
#
#   * Each row contains exactly one of each digit
#   * Each column contains exactly one of each digit
#   * Each bold-outlined group of cells is a cage containing digits which
#     achieve the specified result using the specified mathematical operation:
#       addition (+),
#       subtraction (-),
#       multiplication (x),
#       and division (/).
#       (Unlike in Killer sudoku, digits may repeat within a group.)
#
# ...
# More complex KenKen problems are formed using the principles described
# above but omitting the symbols +, -, x and /, thus leaving them as
# yet another unknown to be determined.
# '''
#
# The solution is:
#
#   5 6 3 4 1 2
#   6 1 4 5 2 3
#   4 5 2 3 6 1
#   3 4 1 2 5 6
#   2 3 6 1 4 5
#   1 2 5 6 3 4
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

#
# Ensure that the sum of the segments in cc == res
#
def calc(sol, cc, x, res):

  if len(cc) == 2:

    # for two operands there may be a lot of variants
    c00, c01 = cc[0]
    c10, c11 = cc[1]
    a = x[c00 - 1, c01 - 1]
    b = x[c10 - 1, c11 - 1]
    sol.add(
         Or(a+b == res,
            a*b == res,
            a*res == b,
            b*res == a,
            a - b == res,
            b - a == res)
        )
  else:
    # res is either sum or product of the segment
    xx = [x[i[0] - 1, i[1] - 1] for i in cc]
    sol.add(Or(
        Sum(xx) == res,
        Product(xx) == res
        ))

def main():

  sol = SolverFor("QF_FD")

  # data

  # size of matrix
  n = 6

  # For a better view of the problem, see
  #  http://en.wikipedia.org/wiki/File:KenKenProblem.svg

  # hints
  #    [sum, [segments]]
  # Note: 1-based
  problem = [
      [11, [[1, 1], [2, 1]]],
      [2, [[1, 2], [1, 3]]],
      [20, [[1, 4], [2, 4]]],
      [6, [[1, 5], [1, 6], [2, 6], [3, 6]]],
      [3, [[2, 2], [2, 3]]],
      [3, [[2, 5], [3, 5]]],
      [240, [[3, 1], [3, 2], [4, 1], [4, 2]]],
      [6, [[3, 3], [3, 4]]],
      [6, [[4, 3], [5, 3]]],
      [7, [[4, 4], [5, 4], [5, 5]]],
      [30, [[4, 5], [4, 6]]],
      [6, [[5, 1], [5, 2]]],
      [9, [[5, 6], [6, 6]]],
      [8, [[6, 1], [6, 2], [6, 3]]],
      [2, [[6, 4], [6, 5]]]]

  num_p = len(problem)

  #
  # variables
  #

  # the set
  x = {}
  for i in range(n):
    for j in range(n):
      x[i, j] = makeIntVar(sol, "x[%i,%i]" % (i, j), 1, n)

  x_flat = [x[i, j] for i in range(n) for j in range(n)]

  #
  # constraints
  #

  # all rows and columns must be unique
  for i in range(n):
    sol.add(Distinct([x[i, j] for j in range(n)]))
    sol.add(Distinct([x[j, i] for j in range(n)]))

  # calculate the segments
  for (res, segment) in problem:
    calc(sol, segment, x, res)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(n):
      for j in range(n):
        print(mod.eval(x[i, j]), end=' ')
      print()
    print()
    getDifferentSolutionMatrix(sol,mod,x,n,n)

  print("num_solutions:", num_solutions)


if __name__ == "__main__":
  main()
