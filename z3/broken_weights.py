#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Broken weights problem in Z3
#
# From http://www.mathlesstraveled.com/?p=701
# '''
# Here's a fantastic problem I recently heard. Apparently it was first
# posed by Claude Gaspard Bachet de Meziriac in a book of arithmetic problems
# published in 1612, and can also be found in Heinrich Dorrie's 100
# Great Problems of Elementary Mathematics.
#
#     A merchant had a forty pound measuring weight that broke
#     into four pieces as the result of a fall. When the pieces were
#     subsequently weighed, it was found that the weight of each piece
#     was a whole number of pounds and that the four pieces could be
#     used to weigh every integral weight between 1 and 40 pounds. What
#     were the weights of the pieces?
#
# Note that since this was a 17th-century merchant, he of course used a
# balance scale to weigh things. So, for example, he could use a 1-pound
# weight and a 4-pound weight to weigh a 3-pound object, by placing the
# 3-pound object and 1-pound weight on one side of the scale, and
# the 4-pound weight on the other side.
# '''
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from z3_utils_hakank import *



def main(m=40, n=4):

  sol = Solver()
  # sol = Optimize()

  # data
  print('total weight (m):', m)
  print('number of pieces (n):', n)
  print()

  # variables
  weights = [makeIntVar(sol, 'weights[%i]' % j, 1, m) for j in range(n)]
  x = {}
  for i in range(m):
    for j in range(n):
      x[i, j] = makeIntVar(sol, 'x[%i,%i]' % (i, j), -1, 1)
  x_flat = [x[i, j] for i in range(m) for j in range(n)]

  #
  # constraints
  #

  # symmetry breaking
  for j in range(1, n):
    sol.add(weights[j - 1] < weights[j])

  sol.add(Sum(weights) == m)

  # Check that all weights from 1 to 40 can be made.
  #
  # Since all weights can be on either side
  # of the side of the scale we allow either
  # -1, 0, or 1 or the weights, assuming that
  # -1 is the weights on the left and 1 is on the right.
  #
  for i in range(m):
    sol.add(i + 1 == Sum([weights[j] * x[i, j]
                                    for j in range(n)]))

  # objective
  # sol.minimize(weights[n-1])

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('weights:   ', end=' ')
    for w in [mod.eval(weights[j]).as_long() for j in range(n)]:
      print('%3i ' % w, end=' ')
    print()
    print('-' * 30)
    for i in range(m):
      print('weight  %2i:' % (i + 1), end=' ')
      for j in range(n):
        print('%3i ' % mod.eval(x[i, j]).as_long(), end=' ')
      print()
    print()
    getLessSolution(sol,mod,weights[n-1])
    
  print()

  print('num_solutions:', num_solutions)


m = 40
n = 4
if __name__ == '__main__':
  if len(sys.argv) > 1:
    m = int(sys.argv[1])
  if len(sys.argv) > 2:
    n = int(sys.argv[2])
  main(m, n)
