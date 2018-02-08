#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Scheduling speakers problem in Z3
#
# From Rina Dechter, Constraint Processing, page 72
# Scheduling of 6 speakers in 6 slots.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *

def main():

  sol = Solver()

  # data

  n = 6  # number of speakers

  # slots available to speak
  available = [
                          # Reasoning:
      [3, 4, 5, 6],       # 2) the only one with 6 after speaker F -> 1
      [3, 4],             # 5) 3 or 4
      [2, 3, 4, 5],       # 3) only with 5 after F -> 1 and A -> 6
      [2, 3, 4],          # 4) only with 2 after C -> 5 and F -> 1
      [3, 4],             # 5) 3 or 4
      [1, 2, 3, 4, 5, 6]  # 1) the only with 1
  ]

  #
  # variables
  #
  x = [makeIntVar(sol, 'x[%i]' % i, 1, n) for i in range(n)]

  #
  # constraints
  #
  sol.add(Distinct([x[i] for i in range(n)]))

  # ensure that the selected speakers is in the available slot
  for i in range(n):
    member_of(sol, x[i], available[i])

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('x:', [mod.eval(x[i]) for i in range(n)])
    sol.add(Or([x[i] != mod.eval(x[i]) for i in range(n)]))

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
