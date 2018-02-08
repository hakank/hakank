#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# SEND+MORE=MONEY in 'any' base in Z3
#
# Alphametic problem SEND+MORE=MONEY in any base.
#
# Examples:
# Base 10 has one solution:
#   {9, 5, 6, 7, 1, 0, 8, 2}
#
# Base 11 has three soltutions:
#  {10, 5, 6, 8, 1, 0, 9, 2}
#  {10, 6, 7, 8, 1, 0, 9, 3}
#  {10, 7, 8, 6, 1, 0, 9, 2}
#
# Note: there's no limit on the size of the base.
# E.g. Z3 happily solves for base=12345
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *


def main(base=10):

  sol = Solver()

  # data
  print('base:', base)


  # declare variables
  s = makeIntVar(sol,"s",0, base-1)
  e = makeIntVar(sol,"e",0, base-1)
  n = makeIntVar(sol,"n",0, base-1)
  d = makeIntVar(sol,"d",0, base-1)
  m = makeIntVar(sol,"m",0, base-1)
  o = makeIntVar(sol,"o",0, base-1)
  r = makeIntVar(sol,"r",0, base-1)
  y = makeIntVar(sol,"y",0, base-1)

  x = [s,e,n,d,m,o,r,y]
  size = len(x)

  #
  # constraints
  #
  sol.add(Distinct([x[i] for i in range(size)]))
  sol.add(s * base ** 3 + e * base ** 2 + n * base + d +
             m * base ** 3 + o * base ** 2 + r * base + e ==
             m * base ** 4 + o * base ** 3 + n * base ** 2 + e * base + y)
  sol.add(s > 0)
  sol.add(m > 0)

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx = [mod.eval(x[i]).as_long() for i in range(size)]
    print('x:', xx)
    sol.add(Or([x[i] != xx[i] for i in range(size)]))
    

  print('num_solutions:', num_solutions)
  print()


base = 10
if __name__ == '__main__':
  if len(sys.argv) > 1:
    base = int(sys.argv[1])
  main(base)
  if len(sys.argv) > 2:
    for b in range(10,1+int(sys.argv[2])):
      main(b)
