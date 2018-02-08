#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# toNum in Z3
#
# Convert a number <-> array of int in a specific base.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *

# converts a number (s) <-> an array of integers (t) in the specific base.
# def toNum(sol, t, s, base):
#   tlen = len(t)
#   sol.add(
#       s == Sum([(base ** (tlen - i - 1)) * t[i] for i in range(tlen)]))


def main(n=4,base=10):
  # Create the solver.
  sol = Solver()

  # declare variables
  x = [Int("x%i" % i) for i in range(n)]
  for i in range(n):
      sol.add(x[i] >= 0, x[i] <= base-1)
  y = Int("y")
  sol.add(y >= 0, y <= 10**n-1)

  # toNum is defined in z3_utils_hakank
  toNum(sol, x, y, base)

  # some other constraints for fun
  sol.add(Distinct([x[i] for i in range(n)]))
  sol.add(x[0] > 0)
  sol.add(x[1] < x[n-1])

  # solution
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx = [mod.eval(x[i]) for i in range(n)]
    print("x:", xx)
    print("y:", mod.eval(y))
    print()
    sol.add(Or([x[i] != xx[i] for i in range(n)]))

  print("num_solutions:", num_solutions)

n = 4
base = 10
if __name__ == "__main__":
  if len(sys.argv) > 1:
      n = int(sys.argv[1])
  if len(sys.argv) > 2:
      base = int(sys.argv[2])
  main(n, base)
