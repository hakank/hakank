#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Capital budgeting in Z3
#
# Winston Operations Research, page 478: Capital budgeting 
#
# and some extra constraint (page 479):
#
# either one of: 
#  - can only make two investments
#  - if investment 2 then investment 1
#  - if investment 2 then not investment 4
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

# data

budget = 14
npv = [16,22,12,8]
cash_flow = [5,7,4,3]
n = 4

# variables

x = makeIntVector(sol,"x",n,0,1) # x[i] = 1 if investments i
z = Int("z")

# constraints

# the sum of all choosen investments must be less than the budget
sol.add(Sum([x[i]*cash_flow[i] for i in range(n)]) <= budget)

sol.add(z == sum([(x[i]*npv[i])  for i in range(n)]))

# the extra constraints

# only two investments
# sol.add(Sum(x) == 2)

# if investment 2 -> investment 1
# sol.add(Implies(x[1] == 1, x[0] == 1))
## sol.add(x[0] >= x[1])      # alternative (integer programming) way 

# if investment 2 then not investment 4
sol.add(Implies(x[1] == 1, x[3] == 0)) 
## sok.add(x[3] + x[1] <= 1)  # IP way

while sol.check() == sat:
    mod = sol.model()
    print("z:", mod[z])
    print("x:", [mod[x[i]] for i in range(n)])
    getGreaterSolution(sol,mod,z)

