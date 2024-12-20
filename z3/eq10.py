#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Eq 10 problem in Z3
#
# Standard benchmark problem.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *




def main():

  solver = SimpleSolver()

  #
  # data
  #
  n = 7

  #
  # variables
  #
  X = [makeIntVar(solver, "X(%i)" % i, 0, 10) for i in range(n)]
  X1, X2, X3, X4, X5, X6, X7 = X

  #
  # constraints
  #
  solver.add(0 + 98527 * X1 + 34588 * X2 + 5872 * X3 + 59422 * X5 + 65159 * X7
             == 1547604 + 30704 * X4 + 29649 * X6)

  solver.add(
      0 + 98957 * X2 + 83634 * X3 + 69966 * X4 + 62038 * X5 + 37164 * X6 + 85413 * X7 == 1823553 + 93989 *
      X1)

  solver.add(900032 + 10949 * X1 + 77761 * X2 + 67052 * X5
             == 0 + 80197 * X3 + 61944 * X4 + 92964 * X6 + 44550 * X7)

  solver.add(0 + 73947 * X1 + 84391 * X3 + 81310 * X5
             == 1164380 + 96253 * X2 + 44247 * X4 + 70582 * X6 + 33054 * X7)

  solver.add(0 + 13057 * X3 + 42253 * X4 + 77527 * X5 + 96552 * X7
             == 1185471 + 60152 * X1 + 21103 * X2 + 97932 * X6)

  solver.add(1394152 + 66920 * X1 + 55679 * X4 ==
             0 + 64234 * X2 + 65337 * X3 + 45581 * X5 + 67707 * X6 + 98038 * X7)

  solver.add(0 + 68550 * X1 + 27886 * X2 + 31716 * X3 + 73597 * X4 + 38835 * X7
             == 279091 + 88963 * X5 + 76391 * X6)

  solver.add(0 + 76132 * X2 + 71860 * X3 + 22770 * X4 + 68211 * X5 + 78587 * X6
             == 480923 + 48224 * X1 + 82817 * X7)

  solver.add(519878 + 94198 * X2 + 87234 * X3 + 37498 * X4
             == 0 + 71583 * X1 + 25728 * X5 + 25495 * X6 + 70023 * X7)

  solver.add(361921 + 78693 * X1 + 38592 * X5 + 38478 * X6
             == 0 + 94129 * X2 + 43188 * X3 + 82528 * X4 + 69025 * X7)

  num_solutions = 0
  while solver.check() == sat:
    num_solutions += 1
    mod = solver.model()
    print("X:", [mod.eval(X[i]) for i in range(n)])
    print()
    getDifferentSolution(solver,mod,X)

  print()
  print("num_solutions:", num_solutions)


if __name__ == "__main__":
  main()
