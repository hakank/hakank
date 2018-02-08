#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Divisible by 9 through 1 puzzle in Z3
#
# From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
# ' Solving Combinatory Problems with LINQ'
# '''
# Find a number consisting of 9 digits in which each of the digits
# from 1 to 9 appears only once. This number must also satisfy these
# divisibility requirements:
#
#  1. The number should be divisible by 9.
#  2. If the rightmost digit is removed, the remaining number should
#     be divisible by 8.
#  3. If the rightmost digit of the new number is removed, the remaining
#     number should be divisible by 7.
#  4. And so on, until there's only one digit (which will necessarily
#     be divisible by 1).
# '''
#
# Also, see
# 'Intel Parallel Studio: Great for Serial Code Too (Episode 1)'
# http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *

def main(base=10):

  # Create the solver.
  sol = Solver()

  # data
  m = base ** (base - 1) - 1
  n = base - 1
  digits_str = "_0123456789ABCDEFGH"

  print("base:", base)

  # declare variables

  # the digits
  x = [Int("x[%i]" % i) for i in range(n)]
  # the numbers where t[0] contains the answer
  t = [Int("t[%i]" % i) for i in range(n)]
  for i in range(n):
      sol.add(x[i] >= 1, x[i] <= base-1)
      sol.add(t[i] >= 0, t[i] <= m)      

  # constraints
  sol.add(Distinct([x[i] for i in range(n)]))

  for i in range(n):
    mm = base-i-1
    toNum(sol, [x[j] for j in range(mm)], t[i], base)
    sol.add(t[i] % mm == 0) # nice that z3 support modulo!

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx = [mod.eval(x[i]).as_long() for i in range(n)]
    tt = [mod.eval(t[i]).as_long() for i in range(n)]
    print("x: ", xx)
    print("t: ", tt)
    print("number base 10: %i base %i: %s" % (tt[0],
                                              base,
                                              "".join([digits_str[xx[i]+1] for i in range(n)])))
    sol.add(Or([x[i] != xx[i] for i in range(n)]))
    print()

  print("num_solutions:", num_solutions)


base = 10
if __name__ == "__main__":
  if len(sys.argv) > 1:
    base = int(sys.argv[1])
  main(base)

# for base in range(2, 17):
#      main(base)
#      print()
