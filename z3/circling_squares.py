#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Circling the Squares puzzle in Z3
#
# From the Oz examples
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/circlingsquares.html
# """
# from "Amusements in Mathematics, Dudeney",
# number 43.
#
# The puzzle is to place a different number in each of the ten squares
# so that the sum of the squares of any two adjacent numbers shall be
# equal to the sum of the squares of the two numbers diametrically
# opposite to them. The four numbers placed, as examples, must stand as
# they are. Fractions are not allowed, and no number need contain more
# than two figures.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

def s(sol, x1,x2,y1,y2):
  sol.add(x1*x1 + x2*x2 == y1*y1 + y2*y2)


sol = SolverFor("QF_LIA") # 10.8s
# sol = Solver() # 16.2s

n = 10

# variables
LD = makeIntVector(sol,"LD",10,1,99)
[A,B,C,D,E,F,G,H,I,K] = LD

# constraints
sol.add(Distinct(LD))
sol.add(A == 16)
sol.add(B == 2)
sol.add(F == 8)
sol.add(G == 14)

s(sol, A, B, F, G)
s(sol, B, C, G, H)
s(sol, C, D, H, I)
s(sol, D, E, I, K)
s(sol, E, F, K, A)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print([mod.eval(LD[i]) for i in range(n)])
  getDifferentSolution(sol,mod,LD)

print("num_solutions:", num_solutions)


