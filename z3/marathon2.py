#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Marathon puzzle in Z3
#
# From Xpress example
# http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
# '''
# Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
# have arrived as the first six at the Paris marathon.
# Reconstruct their arrival order from the following
# information:
# a) Olivier has not arrived last
# b) Dominique, Pascal and Ignace have arrived before Naren
#    and Olivier
# c) Dominique who was third last year has improved this year.
# d) Philippe is among the first four.
# e) Ignace has arrived neither in second nor third position.
# f) Pascal has beaten Naren by three positions.
# g) Neither Ignace nor Dominique are on the fourth position.
#
#    (c) 2002 Dash Associates
#   author: S. Heipcke, Mar. 2002
# '''
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Solver()

  # data
  n = 6

  runners_str = ['Dominique', 'Ignace', 'Naren',
                 'Olivier', 'Philippe', 'Pascal']

  #
  # declare variables
  #
  runners = [makeIntVar(sol, 'runners[%i]' % i, 1, n) for i in range(n)]
  Dominique, Ignace, Naren, Olivier, Philippe, Pascal = runners

  #
  # constraints
  #
  sol.add(Distinct(runners))

  # a: Olivier not last
  sol.add(Olivier != n)

  # b: Dominique, Pascal and Ignace before Naren and Olivier
  sol.add(Dominique < Naren)
  sol.add(Dominique < Olivier)
  sol.add(Pascal < Naren)
  sol.add(Pascal < Olivier)
  sol.add(Ignace < Naren)
  sol.add(Ignace < Olivier)

  # c: Dominique better than third
  sol.add(Dominique < 3)

  # d: Philippe is among the first four
  sol.add(Philippe <= 4)

  # e: Ignace neither second nor third
  sol.add(Ignace != 2)
  sol.add(Ignace != 3)

  # f: Pascal three places earlier than Naren
  sol.add(Pascal + 3 == Naren)

  # g: Neither Ignace nor Dominique on fourth position
  sol.add(Ignace != 4)
  sol.add(Dominique != 4)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    runners_val = [mod.eval(runners[i]) for i in range(n)]
    print('runners:', runners_val)
    print('Places:')
    for i in range(1, n + 1):
      for j in range(n):
        if runners_val[j].as_long() == i:
          print('%i: %s' % (i, runners_str[j]))
    print()
    getDifferentSolution(sol,mod,runners)

  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()


