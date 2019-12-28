#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Monks and doors problem in Z3
#
# From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
# """
# There is a room with four doors and eight monks. One or more of
# the doors may be exit. Each monk is either telling a lie or the truth.
# 
# The monks make the following statements:
# Monk 1: Door A is the exit.
# Monk 2: At least one of the doors B and C is the exit.
# Monk 3: Monk 1 and Monk 2 are telling the truth.
# Monk 4: Doors A and B are both exits.
# Monk 5: Doors A and B are both exits.
# Monk 6: Either Monk 4 or Monk 5 is telling the truth.
# Monk 7: If Monk 3 is telling the truth, so is Monk 6.
# Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
# 
# Which door is an exit no matter who is a liar and who is telling the
# truth.
# """
# 
# Answer: Door A is an exit.
#         And monks 1, 7, and 8 are telling the truth.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

# variables
A,B,C,D = Bools("A B C D") # Doors
doors = [A,B,C,D] 
M1,M2,M3,M4,M5,M6,M7,M8 = Bools("M1 M2 M3 M4 M5 M6 M7 M8") # monks
monks = [M1,M2,M3,M4,M5,M6,M7,M8]

# constraints

# Monk 1: Door A is the exit.
sol.add(M1 == A)

# Monk 2: At least one of the doors B and C is the exit.
sol.add(M2 == (If(B,1,0) + If(C,1,0) >= 1))

# Monk 3: Monk 1 and Monk 2 are telling the truth.
sol.add(M3 == And(M1, M2)) 

# Monk 4: Doors A and B are both exits.
sol.add(M4 == And(A,B))

# Monk 5: Doors A and C are both exits.
sol.add(M5 == And(A, C))

# Monk 6: Either Monk 4 or Monk 5 is telling the truth.
sol.add(M6 == Or(M4,M5))

# Monk 7: If Monk 3 is telling the truth, so is Monk 6.
sol.add(M7 == Implies(M3, M6))

# Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
sol.add(M8 == (Implies(And(M7, M8),M1)))

# Exactly one door is an exit.
sol.add(If(A,1,0) + If(B,1,0) + If(C,1,0) + If(D,1,0) == 1)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("doors:", [mod.eval(D) for D in doors])
  print("monks:", [mod.eval(M) for M in monks])
  getDifferentSolution(sol,mod,doors,monks)

print("num_solutions:", num_solutions)


