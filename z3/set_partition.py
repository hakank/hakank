#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Set partition problem in Z3
#
# Problem formulation from
# http://www.koalog.com/resources/samples/PartitionProblem.java.html
# '''
# This is a partition problem.
# Given the set S = {1, 2, ..., n},
# it consists in finding two sets A and B such that:
#    A U B = S,
#    |A| = |B|,
#    sum(A) = sum(B),
#    sum_squares(A) = sum_squares(B)
#
# '''
#
#  This model uses a binary matrix to represent the sets.

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *


#
# Partition the sets (binary matrix representation).
#
def partition_sets(sol, x, num_sets, n):
  for i in range(num_sets):
    for j in range(num_sets):
      if i != j:
        sol.add(Sum([x[i, k] * x[j, k] for k in range(n)]) == 0)

  # ensure that all integers is in
  # (exactly) one partition
  sol.add(Sum([x[i, j] for i in range(num_sets) for j in range(n)]) == n)


def main(n=16, num_sets=2):

  # Create the sol.
  sol = SolverFor("QF_FD")

  #
  # data
  #
  print("n:", n)
  print("num_sets:", num_sets)
  print()

  # Check sizes
  assert n % num_sets == 0, "Equal sets is not possible."

  #
  # variables
  #

  # the set
  a = {}
  for i in range(num_sets):
    for j in range(n):
      a[i, j] = makeIntVar(sol, "a[%i,%i]" % (i, j), 0, 1)

  a_flat = [a[i, j] for i in range(num_sets) for j in range(n)]

  #
  # constraints
  #

  # partition set
  partition_sets(sol, a, num_sets, n)

  for i in range(num_sets):
    for j in range(i, num_sets):

      # same cardinality
      sol.add(Sum([a[i, k] for k in range(n)])
              ==
              Sum([a[j, k] for k in range(n)]))

      # same sum
      sol.add(Sum([k * a[i, k] for k in range(n)])
              ==
              Sum([k * a[j, k] for k in range(n)]))

      # same sum squared
      sol.add(Sum([(k * a[i, k]) * (k * a[i, k])
                             for k in range(n)])
              ==
              Sum([(k * a[j, k]) * (k * a[j, k])
                             for k in range(n)]))

  # symmetry breaking for num_sets == 2
  if num_sets == 2:
    sol.add(a[0, 0] == 1)

  # search and result
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    a_val = {}
    for i in range(num_sets):
      for j in range(n):
        a_val[i, j] = mod.eval(a[i, j]).as_long()

    sq = sum([(j + 1) * a_val[0, j] for j in range(n)])
    print("sums:", sq)
    sq2 = sum([((j + 1) * a_val[0, j]) ** 2 for j in range(n)])
    print("sums squared:", sq2)

    for i in range(num_sets):
      if sum([a_val[i, j] for j in range(n)]):
        print(i + 1, ":", end=' ')
        for j in range(n):
          if a_val[i, j] == 1:
            print(j + 1, end=' ')
        print()
    sol.add(Or([a[i,j] != mod.eval(a[i,j]) for i in range(num_sets) for j in range(n)]))
    print()

  print()
  print("num_solutions:", num_solutions)

n = 16
num_sets = 2
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  if len(sys.argv) > 2:
    num_sets = int(sys.argv[2])

  main(n, num_sets)
