#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Pandigital numbers in Z3
#
# From Albert H. Beiler 'Recreations in the Theory of Numbers',
# quoted from http://www.worldofnumbers.com/ninedig1.htm
# '''
# Chapter VIII : Digits - and the magic of 9
#
# The following curious table shows how to arrange the 9 digits so that
# the product of 2 groups is equal to a number represented by the
# remaining digits.
#
#    12 x 483 = 5796
#    42 x 138 = 5796
#    18 x 297 = 5346
#    27 x 198 = 5346
#    39 x 186 = 7254
#    48 x 159 = 7632
#    28 x 157 = 4396
#    4 x 1738 = 6952
#    4 x 1963 = 7852
# '''
#
# See also MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
# '''
# A number is said to be pandigital if it contains each of the digits
# from 0 to 9 (and whose leading digit must be nonzero). However,
# 'zeroless' pandigital quantities contain the digits 1 through 9.
# Sometimes exclusivity is also required so that each digit is
# restricted to appear exactly once.
# '''
#
# * Wikipedia http://en.wikipedia.org/wiki/Pandigital_number
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main(base=10, start=1, len1=1, len2=4):

  sol = Solver()

  # data
  max_d = base - 1
  x_len = max_d + 1 - start
  max_num = base ** 4 - 1

  # declare variables
  num1 = makeIntVar(sol,"num1",0, max_num)
  num2 = makeIntVar(sol,"num2",0, max_num)
  res = makeIntVar(sol, "res", 0, max_num)

  x = [makeIntVar(sol, "x[%i]" % i, start, max_d) for i in range(x_len)]

  #
  # constraints
  #
  sol.add(Distinct(x))

  toNum(sol, [x[i] for i in range(len1)], num1, base)
  toNum(sol, [x[i] for i in range(len1, len1 + len2)], num2, base)
  toNum(sol, [x[i] for i in range(len1 + len2, x_len)], res, base)

  sol.add(num1 * num2 == res)

  # no number must start with 0
  sol.add(x[0] > 0)
  sol.add(x[len1] > 0)
  sol.add(x[len1 + len2] > 0)

  # symmetry breaking
  sol.add(num1 < num2)

  num_solutions = 0
  solutions = []
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print_solution([mod.eval(x[i]) for i in range(x_len)], len1, len2, x_len)
    getDifferentSolution(sol,mod,x)

def print_solution(x, len1, len2, x_len):
  print("".join([str(x[i]) for i in range(len1)]), "*", end=' ')
  print("".join([str(x[i]) for i in range(len1, len1 + len2)]), "=", end=' ')
  print("".join([str(x[i]) for i in range(len1 + len2, x_len)]))


base = 10
start = 1
if __name__ == "__main__":
  if len(sys.argv) > 1:
    base = int(sys.argv[1])
  if len(sys.argv) > 2:
    start = int(sys.argv[2])

  x_len = base - 1 + 1 - start
  for len1 in range(1 + (x_len)):
    for len2 in range(1 + (x_len)):
      if x_len > len1 + len2:
        main(base, start, len1, len2)
