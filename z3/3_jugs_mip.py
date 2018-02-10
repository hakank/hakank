#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# 3 jugs problem using MIP modeling in Z3
#
# A.k.a. water jugs problem.
#
# Problem from Taha 'Introduction to Operations Research',
# page 245f .
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
  n = 15
  start = 0  # start node
  end = 14  # end node
  M = 999   # a large number

  nodes = ['8,0,0',  # start
           '5,0,3',
           '5,3,0',
           '2,3,3',
           '2,5,1',
           '7,0,1',
           '7,1,0',
           '4,1,3',
           '3,5,0',
           '3,2,3',
           '6,2,0',
           '6,0,2',
           '1,5,2',
           '1,4,3',
           '4,4,0'  # goal!
          ]

  # distance
  d = [[M, 1, M, M, M, M, M, M, 1, M, M, M, M, M, M],
       [M, M, 1, M, M, M, M, M, M, M, M, M, M, M, M],
       [M, M, M, 1, M, M, M, M, 1, M, M, M, M, M, M],
       [M, M, M, M, 1, M, M, M, M, M, M, M, M, M, M],
       [M, M, M, M, M, 1, M, M, 1, M, M, M, M, M, M],
       [M, M, M, M, M, M, 1, M, M, M, M, M, M, M, M],
       [M, M, M, M, M, M, M, 1, 1, M, M, M, M, M, M],
       [M, M, M, M, M, M, M, M, M, M, M, M, M, M, 1],
       [M, M, M, M, M, M, M, M, M, 1, M, M, M, M, M],
       [M, 1, M, M, M, M, M, M, M, M, 1, M, M, M, M],
       [M, M, M, M, M, M, M, M, M, M, M, 1, M, M, M],
       [M, 1, M, M, M, M, M, M, M, M, M, M, 1, M, M],
       [M, M, M, M, M, M, M, M, M, M, M, M, M, 1, M],
       [M, 1, M, M, M, M, M, M, M, M, M, M, M, M, 1],
       [M, M, M, M, M, M, M, M, M, M, M, M, M, M, M]]

  #
  # variables
  #

  # requirements (right hand statement)
  rhs = [makeIntVar(sol, 'rhs[%i]' % i, -1, 1) for i in range(n)]

  x = {}
  for i in range(n):
    for j in range(n):
      x[i, j] = makeIntVar(sol, 'x[%i,%i]' % (i, j) ,0, 1)

  out_flow = [makeIntVar(sol, 'out_flow[%i]' % i,0, 1) for i in range(n)]
  in_flow = [makeIntVar(sol, 'in_flow[%i]' % i, 0, 1) for i in range(n)]

  # length of path, to be minimized
  z = Int("z")

  #
  # constraints
  #
  sol.add(z == Sum([d[i][j] * x[i, j]
                    for i in range(n)
                    for j in range(n) if d[i][j] < M]))

  for i in range(n):
    if i == start:
      sol.add(rhs[i] == 1)
    elif i == end:
      sol.add(rhs[i] == -1)
    else:
      sol.add(rhs[i] == 0)

  # outflow constraint
  for i in range(n):
    sol.add(out_flow[i] == Sum([x[i, j]
                                   for j in range(n)
                                   if d[i][j] < M]))

  # inflow constraint
  for j in range(n):
    sol.add(in_flow[j] == Sum([x[i, j]
                                  for i in range(n)
                                  if d[i][j] < M]))

  # inflow = outflow
  for i in range(n):
    sol.add(out_flow[i] - in_flow[i] == rhs[i])

  # objective
  # sol.minimize(z)

  #
  # solution and search
  #
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print()
    print('z: ', mod.eval(z))
    getLessSolution(sol,mod,z)

  t = start
  while t != end:
    print(nodes[t], '->', end=' ')
    for j in range(n):
      if mod.eval(x[t, j]).as_long() == 1:
        print(nodes[j])
        t = j
        break

  print()
  print("num_solutions:", num_solutions)

if __name__ == '__main__':
  main()
