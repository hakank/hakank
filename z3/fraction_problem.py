#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Fractions problem in Z3
# Prolog benchmark problem (BProlog)
# """
# Find distinct non-zero digits such that the following equation holds:
#        A        D        G
#     ------  + ----- + ------  = 1
#       B*C      E*F      H*I
# """
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

x = makeIntVector(sol, "x", 9, 1,9)
[A,B,C,D,E,F,G,H,I] = x

D1 = makeIntVar(sol, "D1", 1,81)
D2 = makeIntVar(sol, "D2", 1,81)
D3 = makeIntVar(sol, "D3", 1,81)

sol.add(Distinct(x)) 
sol.add(D1 == 10*B+C)
sol.add(D2 == 10*E+F)
sol.add(D3 == 10*H+I)
sol.add(A*D2*D3 + D*D1*D3 + G*D1*D2 == D1*D2*D3)
# break the symmetry
sol.add(A*D2 >= D*D1)
sol.add(D*D3 >= G*D2)
# redundant constraints
sol.add(3*A >= D1)
sol.add(3*G <= D2)


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("%s/(%s%s) + %s(%s%s) + %s(%s%s) = 1" %( mod.eval(A),mod.eval(B), mod.eval(C),mod.eval(D),mod.eval(E), mod.eval(F),mod.eval(G),mod.eval(H), mod.eval(I)))
  getDifferentSolution(sol,mod, x)

print("num_solutions:", num_solutions)
