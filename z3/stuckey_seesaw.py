#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Balancing on a seesaw in Z3
#
# From  Marriott & Stuckey "Programming with Constraints", page 257.
# """
# Suppose that Liz, Fi, and Sarah are playing on a 10 foot long seesaw
# which has seats placed uniformly one foot apart along the bar. ...
# They wish to position themselves on the seesaw to that it balances.
# The also wish to be able to swing their arms freely requiring that
# they are at least three feet apart. The weights of Liz, Fi, and Sarah
# are respectively 9, 8, and 4 stone.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

# ensure that x and y are n feet apart
def apart(sol, x, y, n):
    sol.add(Or(x >= y + n, y >= x + n))


sol = Solver()

n = 3

# variables
people = makeIntVector(sol,"people", n, -5,5)
Liz, Fi, Sara = people


# constraints
sol.add(9*Liz + 8* Fi + 4*Sara == 0)
apart(sol, Liz, Fi, 3)
apart(sol, Liz, Sara, 3)
apart(sol, Sara, Fi, 3)
# symmetry breaking
# sol.add(Sara >= 0)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "Liz, Fi, Sarah: ", [mod.eval(people[i]) for i in range(n)]
  getDifferentSolution(sol,mod,people)

print "num_solutions:", num_solutions

