#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Olympic puzzle in Z3
#
# Benchmark for Prolog (BProlog)
# '''
# File   : olympic.pl
# Author : Neng-Fa ZHOU
# Date   : 1993
#
# Purpose: solve a puzzle taken from Olympic Arithmetic Contest
#
# Given ten variables with the following configuration:
#
#                X7   X8   X9   X10
#                   X4   X5   X6
#                      X2   X3
#                         X1
#
# We already know that X1 is equal to 3 and want to assign each variable
# with a different integer from {1,2,...,10} such that for any three
# variables
#                       Xi   Xj
#                          Xk
# the following constraint is satisfied:
#
#                     |Xi-Xj| = Xk
# '''

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from z3_utils_hakank import *


def minus(sol, x, y, z):
  sol.add(z == Abs(x - y))


def main():

  sol = Solver()

  #
  # data
  #
  n = 10

  #
  # declare variables
  #
  Vars = [makeIntVar(sol, 'Vars[%i]' % i, 1, n) for i in range(n)]
  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10 = Vars

  #
  # constraints
  #
  sol.add(Distinct(Vars))

  sol.add(X1 == 3)
  minus(sol, X2, X3, X1)
  minus(sol, X4, X5, X2)
  minus(sol, X5, X6, X3)
  minus(sol, X7, X8, X4)
  minus(sol, X8, X9, X5)
  minus(sol, X9, X10, X6)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('Vars:', [mod.eval(Vars[i]) for i in range(n)])
    getDifferentSolution(sol,mod,Vars)

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
