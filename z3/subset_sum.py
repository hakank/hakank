#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Subset sum problem in Z3
#
# From Katta G. Murty: 'Optimization Models for Decision Making', page 340
# http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
# '''
# Example 7.8.1

# A bank van had several bags of coins, each containing either
# 16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
# street, thieves stole some bags. A total of 100 coins were lost.
# It is required to find how many bags were stolen.
# '''

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *


def main(coins, total):

  # sol = Solver()
  sol = SolverFor("QF_FD")

  # data
  print("coins:", coins)
  print("total:", total)
  print()

  # declare variables

  # constraints
  x, ss = subset_sum(sol, coins, total)

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    n = len(x)
    xx = [mod.eval(x[i]).as_long() for i in range(n)]
    print("ss:", mod.eval(ss))
    print("x: ", xx)
    print()
    sol.add(Or([x[i] != xx[i] for i in range(n)]))

  print("num_solutions:", num_solutions)

coins = [16, 17, 23, 24, 39, 40]
total = 100
if __name__ == "__main__":
  if len(sys.argv) > 1:
    total = int(sys.argv[1])   
  main(coins, total)
