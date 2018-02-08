#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Traffic lights problem in Z3
#
# CSPLib problem 16
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
# '''
# Specification:
# Consider a four way traffic junction with eight traffic lights. Four of the
# traffic
# lights are for the vehicles and can be represented by the variables V1 to V4
# with domains
# {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic
# lights are
# for the pedestrians and can be represented by the variables P1 to P4 with
# domains {r,g}.
#
# The constraints on these variables can be modelled by quaternary constraints
# on
# (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples
# {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
#
# It would be interesting to consider other types of junction (e.g. five roads
# intersecting) as well as modelling the evolution over time of the traffic
# light sequence.
# ...
#
# Results
# Only 2^2 out of the 2^12 possible assignments are solutions.
#
# (V1,P1,V2,P2,V3,P3,V4,P4) =
#    {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r),
#    (y,r,ry,r,y,r,ry,r)}
#    [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1,
#    2,1)}
#
#  The problem has relative few constraints, but each is very tight. Local
#  propagation
# appears to be rather ineffective on this problem.
#
# '''
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *


def main(base=10, start=1, len1=1, len2=4):

  sol = Solver()

  #
  # data
  #
  n = 4
  r, ry, g, y = list(range(n))
  lights = ["r", "ry", "g", "y"]

  # The allowed combinations
  allowed = []
  allowed.extend([[r, r, g, g],
                 [ry, r, y, r],
                 [g, g, r, r],
                 [y, r, ry, r]])

  #
  # declare variables
  #
  V = [makeIntVar(sol,"V[%i]" % i, 0, n - 1) for i in range(n)]
  P = [makeIntVar(sol,"P[%i]" % i, 0, n - 1) for i in range(n)]

  # constraints
  for i in range(n):
    for j in range(n):
      if j == (1 + i) % n:
        allowed_assignments(sol,[V[i], P[i], V[j], P[j]],allowed)

  # print(sol)

  # Search and result
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(n):
      print("%+2s %+2s" % (lights[mod.eval(V[i]).as_long()], lights[mod.eval(P[i]).as_long()]), end=' ')
    print()
    sol.add(
        Or(
           Or([V[i] != mod.eval(V[i]) for i in range(n)]),
           Or([P[i] != mod.eval(P[i]) for i in range(n)])
        )
        )


  print()
  print("num_solutions:", num_solutions)


if __name__ == "__main__":
  main()
