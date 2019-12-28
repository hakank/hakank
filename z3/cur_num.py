#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Curious numbers in Z3
# 
# From Martin Henz' collection of puzzles
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/#curious
# """
# Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.
#
# The number 48 has this peculiarity, that if you add 1 to it the result
# is a square number, and if you add 1 to its half, you also get a
# square number. Now, there is no limit to the numbers that have this
# peculiarity, and it is an interesting puzzle to find three more of
# them---the smallest possible numbers. What are they?
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 6
# variables
up = 4000000; # upper range limit
arr = makeIntVector(sol,"arr",n,1,up)
[X,A,B,C,D,E] = arr

# constraints
sol.add(X + 1 == A) # if you add 1 to it
sol.add(A == B * B) # the result is a square number
sol.add(X == 2 * C) # if you to its half
sol.add(C + 1 == D) # add 1
sol.add(D == E * E) # you also get a square number


num_solutions = 0
cur_nums = []
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  # print("arr:", [mod.eval(arr[i]) for i in range(n)])
  cur_nums.append(mod.eval(arr[0]).as_long())
  getDifferentSolution(sol,mod,arr)

print("num_solutions:", num_solutions)

cur_nums.sort()
print("cur_nums:", cur_nums)



