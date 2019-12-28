#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# The Abbot's Puzzle in Z3
#
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.
# html
# """
# The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.
# 
# If 100 bushels of corn were distributed among 100 people in such a
# manner that each man received three bushels, each woman two, and each
# child half a bushel, how many men, women, and children were there?
# 
# Dudeney added the condition that there are five times as many women as
# men. That way, the solution becomes unique (otherwise, there are seven
# solutions).
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

x = makeIntVector(sol, "x", 3, 0,100)
M, W, C = x

sol.add(100 == M + W + C,
        M * 6 + W * 4 + C == 200,
        M * 5 == W)
       
num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("x:", [mod.eval(x[i]) for i in range(3)])
  getDifferentSolution(sol,mod,x)

print("num_solutions:", num_solutions)


