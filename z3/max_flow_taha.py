#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Max flow problem in Z3
# From Taha 'Introduction to Operations Research', Example 6.4-2
#
# Translated from the AMPL code at
# http://taha.ineg.uark.edu/maxflo.txt

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Solver()

  #
  # data
  #
  n = 5
  start = 0
  end = n - 1

  nodes = list(range(n))

  # cost matrix
  c = [
      [0, 20, 30, 10, 0],
      [0, 0, 40, 0, 30],
      [0, 0, 0, 10, 20],
      [0, 0, 5, 0, 20],
      [0, 0, 0, 0, 0]
  ]

  #
  # declare variables
  #
  x = {}
  for i in nodes:
    for j in nodes:
      x[i, j] = makeIntVar(sol, 'x[%i,%i]' % (i, j), 0, c[i][j])

  x_flat = [x[i, j] for i in nodes for j in nodes]
  out_flow = [makeIntVar(sol, 'out_flow[%i]' % i, 0, 10000) for i in nodes]
  in_flow = [makeIntVar(sol, 'in_flow[%i]' % i, 0, 10000) for i in nodes]

  total = makeIntVar(sol, 'z', 0, 10000)

  #
  # constraints
  #
  sol.add(total == Sum([x[start, j] for j in nodes if c[start][j] > 0]))

  for i in nodes:
    sol.add(in_flow[i] == Sum([x[j, i] for j in nodes if c[j][i] > 0]))
    sol.add(out_flow[i] == Sum([x[i, j] for j in nodes if c[i][j] > 0]))

  # in_flow == out_flow
  for i in nodes:
    if i != start and i != end:
      sol.add(out_flow[i] - in_flow[i] == 0)

  s1 = [x[i, start] for i in nodes if c[i][start] > 0]
  if len(s1) > 0:
    sol.add(Sum([x[i, start]
                           for i in nodes if c[i][start] > 0] == 0))

  s2 = [x[end, j] for j in nodes if c[end][j] > 0]
  if len(s2) > 0:
    sol.add(Sum([x[end, j]
                           for j in nodes if c[end][j] > 0]) == 0)

  # objective: maximize total cost
  # sol.maximize(total)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('total:', mod.eval(total))
    print('in_flow:', [mod.eval(in_flow[i]) for i in nodes])
    print('out_flow:', [mod.eval(out_flow[i]) for i in nodes])
    for i in nodes:
      for j in nodes:
        print('%2i' % mod.eval(x[i, j]).as_long(), end=' ')
      print()
    print()
    getGreaterSolution(sol,mod,total)

  print('num_solutions:', num_solutions)

if __name__ == '__main__':
  main()

