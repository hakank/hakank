#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Generic alphametic solver in Z3
#
# This is a generic alphametic solver.
#
# Usage:
#    python alphametic.py
#                        ->  solves SEND+MORE=MONEY in base 10
#
#    python alphametic.py  'SEND+MOST=MONEY' 11
#                        -> solver SEND+MOST=MONEY in base 11
#
#    python alphametic.py TEST <base>
#                        -> solve some test problems in base <base>
#                           (defined in test_problems())
#
# Assumptions:
# - we only solves problems of the form
#          NUMBER<1>+NUMBER<2>...+NUMBER<N-1> = NUMBER<N>
#   i.e. the last number is the sum
# - the only nonletter characters are: +, =, \d (which are splitted upon)
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
import re
from z3_utils_hakank import *


def main(problem_str="SEND+MORE=MONEY", base=10):


  sol = SolverFor("QF_FD")

  # data
  print("\nproblem:", problem_str)

  # convert to array.
  problem = re.split("[\s+=]", problem_str)

  p_len = len(problem)
  print("base:", base)

  # create the lookup table: list of (digit : ix)
  a = sorted(set("".join(problem)))
  n = len(a)
  lookup = dict(list(zip(a, list(range(n)))))

  # length of each number
  lens = list(map(len, problem))

  #
  # declare variables
  #

  # the digits
  x = [makeIntVar(sol, "x[%i]" % i, 0, base - 1) for i in range(n)]
  # the sums of each number (e.g. the three numbers SEND, MORE, MONEY)
  sums = [makeIntVar(sol, "sums[%i]"%i, 1, 10 ** (lens[i]) - 1) for i in range(p_len)]

  #
  # constraints
  #
  sol.add(Distinct(x))

  ix = 0
  for prob in problem:
    this_len = len(prob)

    # sum all the digits with proper exponents to a number
    sol.add(sums[ix] == Sum(
        [(base ** i) * x[lookup[prob[this_len - i - 1]]] for i in range(this_len)[::-1]]))
    # leading digits must be > 0
    sol.add(x[lookup[prob[0]]] > 0)
    ix += 1

  # the last number is the sum of the previous numbers
  sol.add(Sum([sums[i] for i in range(p_len - 1)]) == sums[-1])

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("\nsolution #%i" % num_solutions)
    for i in range(n):
      print(a[i], "=", mod.eval(x[i]))
    print()
    for prob in problem:
      for p in prob:
        print(p, end=' ')
      print()
    print()
    for prob in problem:
      for p in prob:
        print(mod.eval(x[lookup[p]]), end=' ')
      print()

    print("sums:", [mod.eval(sums[i]) for i in range(p_len)])
    print()
    getDifferentSolution(sol,mod,x)

  print("\nnum_solutions:", num_solutions)


def test_problems(base=10):
  problems = [
      "SEND+MORE=MONEY",
      "SEND+MOST=MONEY",
      "VINGT+CINQ+CINQ=TRENTE",
      "EIN+EIN+EIN+EIN=VIER",
      "DONALD+GERALD=ROBERT",
      "SATURN+URANUS+NEPTUNE+PLUTO+PLANETS",
      "WRONG+WRONG=RIGHT"
  ]

  for p in problems:
    main(p, base)


problem = "SEND+MORE=MONEY"
base = 10
if __name__ == "__main__":
  if len(sys.argv) > 1:
    problem = sys.argv[1]
  if len(sys.argv) > 2:
    base = int(sys.argv[2])

  if problem == "TEST" or problem == "test":
    test_problems(base)
  else:
    main(problem, base)
