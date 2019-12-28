#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Seseman Convent problem in Z3
#
# n is the length of a border
# There are (n-2)^2 "holes", i.e.
# there are n^2 - (n-2)^2 variables to find out.
#
# The simplest problem, n = 3 (n x n matrix)
# which is represented by the following matrix:
#
#  a b c
#  d   e
#  f g h
#
# Where the following constraints must hold:
#
#   a + b + c = border_sum
#   a + d + f = border_sum
#   c + e + h = border_sum
#   f + g + h = border_sum
#   a + b + c + d + e + f = total_sum

# For a (swedish) discussion of this problem, see
# "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
# http://www.hakank.org/webblogg/archives/001084.html
# and
# Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
# (using Eclipse code)
#
# It was also is commented in the (swedish) blog post
# "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
# http://www.hakank.org/webblogg/archives/001209.html

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *


def seseman(n=3):
    
  # Create the solver.
  sol = Solver()

  # data
  border_sum = n * n

  # declare variables
  total_sum = Int("total_sum")
  sol.add(total_sum >= 1, total_sum <= n*n*n*n)
  
  # x[0..n-1,0..n-1]
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = Int("x %i %i" % (i, j))
      sol.add(x[(i, j)] >= 0, x[(i, j)] <= n * n)

  #
  # constraints
  #
  # zero all middle cells
  for i in range(1, n - 1):
    for j in range(1, n - 1):
      sol.add(x[(i, j)] == 0)

  # all borders must be >= 1
  for i in range(n):
    for j in range(n):
      if i == 0 or j == 0 or i == n - 1 or j == n - 1:
        sol.add(x[(i, j)] >= 1)

  # sum the borders (border_sum)
  sol.add(Sum([x[(i, 0)] for i in range(n)]) == border_sum)
  sol.add(Sum([x[(i, n - 1)] for i in range(n)]) == border_sum)
  sol.add(Sum([x[(0, i)] for i in range(n)]) == border_sum)
  sol.add(Sum([x[(n - 1, i)] for i in range(n)]) == border_sum)

  # total
  sol.add(Sum([x[(i, j)] for i in range(n) for j in range(n)]) == total_sum)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("total_sum:", mod.eval(total_sum))
    xx = {}
    for i in range(n):
      for j in range(n):
        xx[(i,j)] = mod.eval(x[(i,j)])
        print(xx[(i,j)],end=" ")
      print()
    print()

    sol.add(Or([xx[(i,j)] != x[(i,j)] for i in range(n) for j in range(n)]))

  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  n = 3
  if len(sys.argv) > 1:
      n = int(sys.argv[1])
  seseman(n)
