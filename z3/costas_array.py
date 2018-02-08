#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Costas array in Z3
# From http://mathworld.wolfram.com/CostasArray.html:
# '''
# An order-n Costas array is a permutation on {1,...,n} such
# that the distances in each row of the triangular difference
# table are distinct. For example, the permutation {1,3,4,2,5}
# has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
# and {4}. Since each row contains no duplications, the permutation
# is therefore a Costas array.
# '''
#
# Also see
# http://en.wikipedia.org/wiki/Costas_array
#
# About this model:
# This model is based on Barry O'Sullivan's model:
# http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn
#
# and my small changes in
# http://hakank.org/minizinc/costas_array.mzn
#
# Since there is no symmetry breaking of the order of the Costas
# array it gives all the solutions for a specific length of
# the array, e.g. those listed in
# http://mathworld.wolfram.com/CostasArray.html
#
# 1     1       (1)
# 2     2       (1, 2), (2,1)
# 3     4       (1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
# 4     12      (1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4),
#               (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1),
#               (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
# ....
#
# See http://www.research.att.com/~njas/sequences/A008404
# for the number of solutions for n=1..
# 1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828,
# 17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052,
# 872, 200, 88, 56, 204,...


# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from z3_utils_hakank import *

def main(n=6):

  sol = Solver()


  # data
  print("n:", n)

  #
  # declare variables
  #
  costas = [makeIntVar(sol, "costas[%i]" % i, 1, n) for i in range(n)]
  differences = {}
  for i in range(n):
    for j in range(n):
      differences[(i, j)] = makeIntVar(sol, "differences[%i,%i]" % (i, j), -n + 1, n - 1)
  differences_flat = [differences[i, j] for i in range(n) for j in range(n)]

  #
  # constraints
  #

  # Fix the values in the lower triangle in the
  # difference matrix to -n+1. This removes variants
  # of the difference matrix for the the same Costas array.
  for i in range(n):
    for j in range(i + 1):
      sol.add(differences[i, j] == -n + 1)

  # hakank: All the following constraints are from
  # Barry O'Sullivans's original model.
  #
  sol.add(Distinct(costas))

  # "How do the positions in the Costas array relate
  #  to the elements of the distance triangle."
  for i in range(n):
    for j in range(n):
      if i < j:
        sol.add(differences[(i, j)] == costas[j] - costas[j - i - 1])

  # "All entries in a particular row of the difference
  #  triangle must be distint."
  for i in range(n - 2):
    sol.add(Distinct([differences[i, j]
                                    for j in range(n) if j > i]))

  #
  # "All the following are redundant - only here to speed up search."
  #

  # "We can never place a 'token' in the same row as any other."
  for i in range(n):
    for j in range(n):
      if i < j:
        sol.add(differences[i, j] != 0)

  for k in range(2, n):
    for l in range(2, n):
      if k < l:
        sol.add(differences[k - 2, l - 1] +
                   differences[k, l] ==
                   differences[k - 1, l - 1] +
                   differences[k - 1, l])

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("costas:", [mod.eval(costas[i]) for i in range(n)])
    print("differences:")
    for i in range(n):
      for j in range(n):
        v = mod.eval(differences[i, j]).as_long()
        if v == -n + 1:
          print("  ", end=' ')
        else:
          print("%2d" % v, end=' ')
      print()
    print()
    getDifferentSolutionMatrix(sol,mod,differences,n,n)

  print()
  print("num_solutions:", num_solutions)

n = 6
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  main(n)

