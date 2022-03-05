#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Simple PERT model in Z3
#
# From Pascal van Hentenryck 
# "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
# http://citeseer.ist.psu.edu/300151.html
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SimpleSolver()

# data
maxTime = 30
n = 11
    #    a  b  c  d  e  f  g  h  j  k  Send 
Times = [7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1]
numDependencies = 15
# Dependencies
# Note: There is no Si
# 1-based (fixed below)
Dependencies = [
  [2,1],  # Sb >= Sa + 7
  [4,1],  # Sd >= Sa + 7
  [3,2],  # Sc >= Sb + 3
  [5,3],  # Se >= Sc + 1
  [5,4],  # Se >= Sd + 8
  [7,3],  # Sg >= Sc + 1
  [7,4],  # Sg >= Sd + 8
  [6,4],  # Sf >= Sd + 8
  [6,3],  # Sf >= Sc + 1
  [8,6],  # Sh >= Sf + 1
  [9,8],  # Sj >= Sh + 3
  [10,7], # Sk >= Sg + 1
  [10,5], # Sk >= Se + 1
  [10,9], # Sk >= Sj + 2
  [11,10] # Send >= Sk + 1
]

# variables
Start = makeIntVector(sol, "Start", n, 0,maxTime) #  when the activity start

SumTimes = makeIntVar(sol,"SumTimes", 0,maxTime*n)

# constraints
sol.add(SumTimes == Sum(Start))

for i in range(numDependencies):
  # also fix for 1-base
  sol.add(Start[Dependencies[i][0]-1] >= Start[Dependencies[i][1]-1] + Times[Dependencies[i][1]-1])

# solve minimize SumTimes; % sum(i in 1..n) (Start[i]);
# solve minimize Start[n]; % minimize Send

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("Time    :", [Times[i] for i in range(n)])
  print("Start   :", [mod.eval(Start[i]) for i in range(n)])
  print("SumTimes:", mod.eval(SumTimes))
  print()
  getLessSolution(sol,mod,Start[n-1])

print("num_solutions:", num_solutions)
