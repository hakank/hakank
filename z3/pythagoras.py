#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Pythagoras in Z3
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

A = Int("A")
B = Int("B")
C = Int("C")

sol.add(
    A >= 1, B >= 1, C >= 1,
    A*A + B*B == C*C,
    A < B, B < C
    )


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print([mod.eval(x) for x in [A,B,C]])
  getDifferentSolution(sol,mod,[A,B,C])

print("num_solutions:", num_solutions)

