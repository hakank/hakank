#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Spreadsheet in Z3
# 
# From Krzysztof Apt "Principles of Constraint Programming" page 16ff. Spreadsheet.
# Cf Winston "Artificial Intelligence", 3rd edition, page 235 
# (not the same values though)
#
# ECLiPSe/ic solver gives the following result:
# B1 = 0.17__0.17
# B4 = 3.5__3.5
# B5 = 1.7__1.7
# C4 = 1.5__1.5
# C5 = 4.5__4.5
# D4 = 5.25__5.25
# D5 = 7.6499999999068677__7.65
# E7 = 12.899999999906868__12.9
# E8 = 15.092999999891033__15.093000000000004
# Total time 0.010s cpu (0.010 setup + 0.000 search)
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

x = makeRealVector(sol,"x", 9, 0, 1000)

B1,B4,B5,C4,C5,D4,D5,E7,E8 = x

sol.add(
      B1 == 0.17,
      B4 == 3.5,
      B5 == 1.7,
      C4 == 1.5,
      C5 == 4.5,
      D4 == B4 * C4,
      D5 == B5 * C5,
      E7 == D4 + D5,
      E8 == E7 * (1.0 + B1)
      )

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("x:", [mod.eval(i).as_decimal(6) for i in x])
  getDifferentSolution(sol,mod,x)

print("num_solutions:", num_solutions)



