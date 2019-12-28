#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# All different modulo in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_modulo.html
# """
# Enforce all variables of the collection VARIABLES to have a distinct 
# rest when divided by M.
# 
# Example
# (<25, 1,14, 3>, 5)
# 
# The equivalence classes associated with values 25, 1, 14 and 3 are 
# respectively equal to 
#    25 mod 5 = 0, 1 mod 5 = 1, 14 mod 5 = 4 and 3 mod 5 = 3. 
# Since they are distinct the alldifferent_modulo constraint holds.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

# data

n = 4
m = 5 # modulo

init = [25,1,14,3]

# variables
x = makeIntVector(sol, "x", n, 1,25)

# for i in range(n): sol.add(x[i] == init[i])

# constraints
all_different_modulo(sol, x, m)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("x:", [mod.eval(x[i]) for i in range(n)])
  getDifferentSolution(sol,mod,x)

print("num_solutions:", num_solutions)

