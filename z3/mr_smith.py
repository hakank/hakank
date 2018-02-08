#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Mr Smith logic problem in Z3
#
# From an IF Prolog example (http://www.ifcomputer.de/)
# '''
# The Smith family and their three children want to pay a visit but they
# do not all have the time to do so. Following are few hints who will go
# and who will not:
#   o If Mr Smith comes, his wife will come too.
#   o At least one of their two sons Matt and John will come.
#   o Either Mrs Smith or Tim will come, but not both.
#   o Either Tim and John will come, or neither will come.
#   o If Matt comes, then John and his father will
#     also come.
# '''
#
# The answer should be:
#   Mr_Smith_comes      =  0
#   Mrs_Smith_comes     =  0
#   Matt_comes          =  0
#   John_comes          =  1
#   Tim_comes           =  1

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
  n = 5

  #
  # declare variables
  #
  x = [Bool('x[%i]' % i) for i in range(n)]
  Mr_Smith, Mrs_Smith, Matt, John, Tim = x

  #
  # constraints
  #

  #
  # I've kept the MiniZinc constraints for clarity
  # and debugging.
  #

  # If Mr Smith comes then his wife will come too.
  # (Mr_Smith -> Mrs_Smith)
  sol.add(Implies(Mr_Smith,Mrs_Smith))

  # At least one of their two sons Matt and John will come.
  # (Matt \/ John)
  sol.add(Or(Matt,John))

  # Either Mrs Smith or Tim will come but not both.
  # bool2int(Mrs_Smith) + bool2int(Tim) = 1 /\
  # (Mrs_Smith xor Tim)
  sol.add(Xor(Mrs_Smith,Tim))

  # Either Tim and John will come or neither will come.
  # (Tim = John)
  sol.add(Tim == John)

  # If Matt comes /\ then John and his father will also come.
  # (Matt -> (John /\ Mr_Smith))
  sol.add(Implies(Matt, And(John,Mr_Smith)))

  #
  # solution and search
  #
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('x:', [mod.eval(x[i]) for i in range(n)])
    getDifferentSolution(sol,mod, x)
    

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
