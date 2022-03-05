#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Discrete tomography in Z3
#
# Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
# '''
# This is a little 'tomography' problem, taken from an old issue
# of Scientific American.
#
# A matrix which contains zeroes and ones gets "x-rayed" vertically and
# horizontally, giving the total number of ones in each row and column.
# The problem is to reconstruct the contents of the matrix from this
# information. Sample run:
#
#   ?- go.
#     0 0 7 1 6 3 4 5 2 7 0 0
#  0
#  0
#  8      * * * * * * * *
#  2      *             *
#  6      *   * * * *   *
#  4      *   *     *   *
#  5      *   *   * *   *
#  3      *   *         *
#  7      *   * * * * * *
#  0
#  0
#
#  Eclipse solution by Joachim Schimpf, IC-Parc
# '''
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *


def discrete_tomography(row_sums="", col_sums=""):

  # Create the sol.
  sol = SolverFor("QF_FD")

  # data
  if row_sums == "":
    print("Using default problem instance")
    row_sums = [0, 0, 8, 2, 6, 4, 5, 3, 7, 0, 0]
    col_sums = [0, 0, 7, 1, 6, 3, 4, 5, 2, 7, 0, 0]

  r = len(row_sums)
  c = len(col_sums)

  # declare variables
  x = []
  for i in range(r):
    t = []
    for j in range(c):
      t.append(makeIntVar(sol, "x[%i,%i]" % (i, j),0, 1))
    x.append(t)
  x_flat = [x[i][j] for i in range(r) for j in range(c)]

  #
  # constraints
  #
  [sol.add(Sum([x[i][j] for j in range(c)]) == row_sums[i]) for i in range(r)]
  [sol.add(Sum([x[i][j] for i in range(r)]) == col_sums[j]) for j in range(c)]

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx = [ mod.eval(x[i][j]).as_long() for i in range(r) for j in range(c) ]
    print_solution(xx, r, c, row_sums, col_sums)
    sol.add(Or([xx[i*c+j] != x[i][j] for i in range(r) for j in range(c)]))
    print()

  print()
  print("num_solutions:", num_solutions)

#
# Print solution
#


def print_solution(x, rows, cols, row_sums, col_sums):
  print("  ", end=' ')
  for j in range(cols):
    print(col_sums[j], end=' ')
  print()
  for i in range(rows):
    print("%2i" % row_sums[i], end=' ')
    for j in range(cols):
      if x[i*cols+j] == 1:
        print("#", end=' ')
      else:
        print(".", end=' ')
    print("")


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  row_sums = f.readline()
  col_sums = f.readline()
  row_sums = [int(r) for r in (row_sums.rstrip()).split(",")]
  col_sums = [int(c) for c in (col_sums.rstrip()).split(",")]

  return [row_sums, col_sums]

if __name__ == "__main__":
  if len(sys.argv) > 1:
    file = sys.argv[1]
    print("Problem instance from", file)
    [row_sums, col_sums] = read_problem(file)
    discrete_tomography(row_sums, col_sums)
  else:
    discrete_tomography()
