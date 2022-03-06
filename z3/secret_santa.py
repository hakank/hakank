#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Secret Santa problem in Z3
#
# From Ruby Quiz Secret Santa
# http://www.rubyquiz.com/quiz2.html
# '''
# Honoring a long standing tradition started by my wife's dad, my friends
# all play a Secret Santa game around Christmas time. We draw names and
# spend a week sneaking that person gifts and clues to our identity. On the
# last night of the game, we get together, have dinner, share stories, and,
# most importantly, try to guess who our Secret Santa was. It's a crazily
# fun way to enjoy each other's company during the holidays.
#
# To choose Santas, we use to draw names out of a hat. This system was
# tedious, prone to many 'Wait, I got myself...' problems. This year, we
# made a change to the rules that further complicated picking and we knew
# the hat draw would not stand up to the challenge. Naturally, to solve
# this problem, I scripted the process. Since that turned out to be more
# interesting than I had expected, I decided to share.
#
# This weeks Ruby Quiz is to implement a Secret Santa selection script.
#
# Your script will be fed a list of names on STDIN.
# ...
# Your script should then choose a Secret Santa for every name in the list.
# Obviously, a person cannot be their own Secret Santa. In addition, my friends
# no longer allow people in the same family to be Santas for each other and your
# script should take this into account.
# '''
#
# Comment: This model skips the file input and mail parts. We
#          assume that the friends are identified with a number from 1..n,
#          and the families is identified with a number 1..num_families.
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
from z3_utils_hakank import *

def main():

  sol = Solver()

  #
  # data
  #
  family = [1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 4, 4]
  num_families = max(family)
  n = len(family)

  #
  # declare variables
  #
  x = makeIntVector(sol,"x", n,0,n-1)

  #
  # constraints
  #
  sol.add(Distinct([x[i] for i in range(n)]))

  # Can't be one own's Secret Santa
  # Ensure that there are no fix-point in the array
  for i in range(n):
    sol.add(x[i] != i)

  # No Secret Santa to a person in the same family
  for i in range(n):
    family_xi = Int(f"family_x[{i}]")
    element(sol, x[i], family, family_xi,n)
    sol.add(family[i] != family_xi)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('x:', [mod.eval(x[i]) for i in range(n)])
    sol.add(Or([x[i] != mod.eval(x[i]) for i in range(n)]))

  print('num_solutions:', num_solutions)

if __name__ == '__main__':
  main()
