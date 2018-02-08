#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Safe cracking puzzle in Z3
#
# From the Oz Primer:
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
# '''
# The code of Professor Smart's safe is a sequence of 9 distinct
# nonzero digits C1 .. C9 such that the following equations and
# inequations are satisfied:
#
#       C4 - C6   =   C7
#  C1 * C2 * C3   =   C8 + C9
#  C2 + C3 + C6   <   C8
#            C9   <   C8
#
#  and
#
#  C1 <> 1, C2 <> 2, ..., C9 <> 9
#
# can you find the correct combination?
# '''
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main():

  sol = Solver()

  #
  # data
  #
  n = 9
  digits = list(range(1, n + 1))

  #
  # variables
  #

  LD = [makeIntVar(sol, 'LD[%i]' % i, 1, n) for i in range(n)]
  C1, C2, C3, C4, C5, C6, C7, C8, C9 = LD

  # constraints
  sol.add(Distinct(LD))

  sol.add(C4 - C6 == C7)
  sol.add(C1 * C2 * C3 == C8 + C9)
  sol.add(C2 + C3 + C6 < C8)
  sol.add(C9 < C8)
  for i in range(n):
    sol.add(LD[i] != i + 1)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('LD:', [mod.eval(LD[i]) for i in range(n)])
    getDifferentSolution(sol,mod, LD)

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
