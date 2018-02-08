#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Some explorations of ISBN13 in Z3
#
# See http://en.wikipedia.org/wiki/ISBN
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

# -1 marks the unknown digit(s)

# calculate the check digit
# given = [9,7,8,3,3,1,9,2,5,8,8,1,-1] # The Picat book 9783319258812

# calculate some other missing digit
given = [9,7,8,-1,3,1,9,2,5,8,8,1,2]   # The Picat book 9783319258812

# Twp missing digits: 10 solutions
# given = [-1,7,8,3,3,1,9,2,5,8,8,-1,2]   # The Picat book 9783319258812


n = 13 # ISBN13

# variables
isbn = makeIntVector(sol, "isbn", n, 0,9)
mult0 = makeIntVar(sol,"mult0", 1,9)
mult1 = makeIntVar(sol,"mult1", 1,9)

# constraints

# copy from given
for i in range(n):
    if given[i] >= 0:
        sol.add(isbn[i] == given[i])

sol.add(mult0 == 3, mult1 == 1)
sol.add(isbn[n-1] == (10 - Sum([
                     If(i % 2 == 0,
                       isbn[i] * mult0 # mult0 = 3
                       ,
                       isbn[i] * mult1 # mult1 = 1
                       )
                     for i in range(n-1)]
                   ) % 10) % 10)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "mult0:", mod.eval(mult0), "mult1:", mod.eval(mult1)
  print "isbn:", [mod.eval(isbn[i]) for i in range(n)]
  print
  getDifferentSolution(sol,mod,isbn)
  
print "num_solutions:", num_solutions


