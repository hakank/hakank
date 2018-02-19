#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Least square optimization problem in Z3
#
# Solving a fourth grade least square equation.
#
# From the Swedish book 'Optimeringslara' [Optimization Theory],
# page 286f.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main():

  sol = Optimize()
  
  # data
  # number of points
  num = 14

  # temperature
  t = [20, 30, 80, 125, 175, 225, 275, 325, 360, 420, 495, 540, 630, 700]

  # percentage gas
  F = [
      0.0, 5.8, 14.7, 31.6, 43.2, 58.3, 78.4, 89.4, 96.4, 99.1, 99.5, 99.9,
      100.0, 100.0]

  p = 4

  #
  # declare variables
  #
  a = [makeRealVar(sol, 'a[%i]' % i, -100.0, 100.0) for i in range(p + 1)]

  # to minimize
  z = makeRealVar(sol,"z",-1000,1000)
  
  #
  # constraints
  #
  sol.add(z == Sum([(F[i] -
                     (Sum([a[j] * t[i] ** j for j in range(p + 1)])))
                    for i in range(num)]))


  sol.add(Sum([20 ** i * a[i] for i in range(p + 1)]) == 0)

  sol.add((a[0] + Sum([(700 ** j) * a[j] for j in range(1, p + 1)])) == 100.0)

  for i in range(num):
    sol.add(Sum([j * a[j] * t[i] ** (j - 1)
                 for j in range(p + 1)]) >= 0)

  sol.minimize(z)

  num_solutions = 0
  if sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print()
    print('z = ', mod.eval(z).as_decimal(6))
    for i in range(p + 1):
      print(mod.eval(a[i]).as_decimal(6), end=' ')
    print()
    # getLessSolution(sol,mod,z)

  print("num_solutions:", num_solutions)


if __name__ == '__main__':
  main()
