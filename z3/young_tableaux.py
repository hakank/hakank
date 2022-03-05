#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Young tableaux in Z3
#   See
#   http://mathworld.wolfram.com/YoungTableau.html
#   and
#   http://en.wikipedia.org/wiki/Young_tableau
#   '''
#   The partitions of 4 are
#   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
#
#   And the corresponding standard Young tableaux are:
#
#   1.   1 2 3 4
#
#   2.   1 2 3         1 2 4    1 3 4
#            4             3        2
#
#   3.   1 2           1 3
#        3 4           2 4
#
#   4    1 2           1 3      1 4
#        3             2        2
#        4             4        3
#
#   5.   1
#        2
#        3
#        4
#   '''
#
#  This is a translation of my Google or-tools model:
#  http://hakank.org/ortools/young_tableaux.py
#  Note from that model: "Thanks to Laurent Perron for improving this model."
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from z3_utils_hakank import *

def young_tableaux(n=5):

  sol = SolverFor("QF_FD")

  # data
  print("n:", n)

  # declare variables
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = Int("x(%i,%i)" % (i, j))
      sol.add(x[(i, j)] >= 1, x[(i,j)] <= n + 1)

  x_flat = [x[(i, j)] for i in range(n) for j in range(n)]

  # partition structure
  p = [Int("p%i" % i) for i in range(n)]
  for i in range(n):
      sol.add(p[i]>= 0, p[i] <= n + 1)

  # constraints

  # 1..n is used exactly once
  for i in range(1, n+1):
    count(sol, i, x_flat, 1)

  sol.add(x[(0, 0)] == 1)

  # row wise
  for i in range(n):
    for j in range(1, n):
      sol.add(x[(i, j)] >= x[(i, j - 1)])

  # column wise
  for j in range(n):
    for i in range(1, n):
      sol.add(x[(i, j)] >= x[(i - 1, j)])

  # calculate the structure (the partition)
  for i in range(n):
    sol.add(p[i] == Sum([ If(x[(i,j)] <= n, 1, 0) for j in range(n)]))

  sol.add(Sum(p) == n)

  for i in range(1, n):
    sol.add(p[i-1] >= p[i])

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    pp = [mod.eval(p[i]).as_long() for i in range(n)]
    print("p:", pp)
    xx = [mod.eval(x_flat[i*n+j]).as_long() for i in range(n) for j in range(n)]
    print("x:")
    for i in range(n):
      for j in range(n):
        val = xx[i*n+j] 
        if val  <= n:
          print(val, end=' ')
      if pp[i] > 0:
        print()
    print()
    getDifferentSolutionMatrix(sol,mod,x,n,n)

  print("count:", num_solutions)


n = 5
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])

  young_tableaux(n)
