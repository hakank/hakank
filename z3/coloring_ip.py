#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Simple coloring problem MIP approach in Z3
#
# Inspired by the GLPK:s model color.mod
# '''
# COLOR, Graph Coloring Problem
#
# Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
#
# Given an undirected loopless graph G = (V, E), where V is a set of
# nodes, E <= V x V is a set of arcs, the Graph Coloring Problem is to
# find a mapping (coloring) F: V -> C, where C = {1, 2, ... } is a set
# of colors whose cardinality is as small as possible, such that
# F(i) != F(j) for every arc (i,j) in E, that is adjacent nodes must
# be assigned different colors.
# '''
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = SolverFor("QF_LIA")

  # max number of colors
  # [we know that 4 suffices for normal maps]
  nc = 5

  # number of nodes
  n = 11
  # set of nodes
  V = list(range(n))

  num_edges = 20

  #
  # Neighbours
  #
  # This data correspond to the instance myciel3.col from:
  # http://mat.gsia.cmu.edu/COLOR/instances.html
  #
  # Note: 1-based (adjusted below)
  E = [[1, 2],
       [1, 4],
       [1, 7],
       [1, 9],
       [2, 3],
       [2, 6],
       [2, 8],
       [3, 5],
       [3, 7],
       [3, 10],
       [4, 5],
       [4, 6],
       [4, 10],
       [5, 8],
       [5, 9],
       [6, 11],
       [7, 11],
       [8, 11],
       [9, 11],
       [10, 11]]

  #
  # declare variables
  #

  # x[i,c] = 1 means that node i is assigned color c
  x = {}
  for v in V:
    for j in range(nc):
      x[v, j] = makeIntVar(sol, 'v[%i,%i]' % (v, j), 0, 1)

  # u[c] = 1 means that color c is used, i.e. assigned to some node
  u = [makeIntVar(sol, 'u[%i]' % i, 0, 1) for i in range(nc)]

  # number of colors used, to minimize
  obj = Int("obj")

  #
  # constraints
  #
  sol.add(obj == Sum(u))

  # each node must be assigned exactly one color
  for i in V:
    sol.add(Sum([x[i, c] for c in range(nc)]) == 1)

  # adjacent nodes cannot be assigned the same color
  # (and adjust to 0-based)
  for i in range(num_edges):
    for c in range(nc):
      sol.add(x[E[i][0] - 1, c] + x[E[i][1] - 1, c] <= u[c])

  # symmetry breaking: assign values from 0..obj-1
  decreasing(sol,u)

  # objective
  # sol.minimize(obj)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions = 0
    mod = sol.model()
    print()
    print('number of colors:', mod.eval(obj))
    print('colors used:', [mod.eval(u[i]) for i in range(nc)])
    print()

    for v in V:
      print('v%i' % v, ' color ', end=' ')
      for c in range(nc):
        if mod.eval(x[v, c]).as_long() == 1:
          print(c)
    getLessSolution(sol,mod,obj)

  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
