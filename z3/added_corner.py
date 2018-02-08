#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Added corner puzzle in Z3
# 
# Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
# """
# This puzzle requires that you enter the digits 1 through 8 in the circles and
# squares (one digit in each figure) so that the number in each square is equal
# to the sum on the numbers in the circles which adjoin it.  
# ...
# 
#    C F C
#    F   F
#    C F C
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 8

L = makeIntVector(sol,"L",n,1,n)
[A,B,C,D,E,F,G,H] = L

# constraints
sol.add(Distinct(L))
sol.add(B == A + C)
sol.add(D == A + F)
sol.add(E == C + H)
sol.add(G == F + H)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print mod.eval(A), "   ", mod.eval(B), "   ", mod.eval(C)
  print mod.eval(D), "         ", mod.eval(E)
  print mod.eval(F), "   ", mod.eval(H), "   ", mod.eval(H)
  print 
  getDifferentSolution(sol,mod,L)

print "num_solutions", num_solutions  
