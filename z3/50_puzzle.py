#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Fifty puzzle (Martin Chlond) in Z3
#
# From Martin Chlond Integer Programming Puzzles:
# http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 5. 
# Description  : Fifty puzzle
# Source       : The Puzzles of Sam Loyd (P 54)
#
# """
# 5. A side show at Coney Island is described as follows: "There were ten little 
# dummies which you were to knock over with baseballs. The man said: 'Take as many 
# throws as you like at a cent apiece and stand as close as you please. Add up the 
# numbers on all the men that you knock down and when the sum amounts to exactly 
# fifty, neither more nor less you get a genuine 25 cent Maggie Cline cigar with 
# a gold band around it.'"

# The numbers on the ten dummies were 15, 9, 30, 21, 19, 3, 12, 6, 25, 27. (Loyd)
# """
#
# Answer: 6, 19 and 25
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 10
v = [3, 6, 9, 12, 15, 19, 21, 25, 27, 30]

x = makeIntVector(sol,"x",n,0,1)
sumX = Int("sumX")

sol.add(sumX == Sum(x))
sol.add(Sum([v[i]*x[i] for i in range(n)]) == 50)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("sumX:", mod[sumX])
  print("x :", [v[i] for i in range(n) if mod[x[i]] == 1])
  print()
  getDifferentSolution(sol,mod,x)

print("num_solutions:", num_solutions)
