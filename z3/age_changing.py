#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Enigma 1224 - Age-changing in Z3

# From Enigma 1224: Age-changing
# https://enigmaticcode.wordpress.com/2015/06/20/enigma-1224-age-changing/
# """
# From New Scientist #2380, 1st February 2003
#
#   If you start with my age, in years, and apply the four operations:
#
#   [  
#      +2  /8 
#
#      -3  *7
#
#   ]
#
#   in some order, then the final answer you get is my husband's age in years.
#
#   Funnily enough, if you start with his age and apply the same four operations in a 
#   different order, then you get my age.
#
#   What are our two ages?
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def check(sol,perm, old, new):
   sol.add(Implies(perm == 0, new == old + 2))
   sol.add(Implies(perm == 1, new == old / 8))
   sol.add(Implies(perm == 2, new == old - 3))
   sol.add(Implies(perm == 3, new == old * 7))

sol = SolverFor("QF_NIA")
# sol = Solver()

n = 4

perms = ["+2","/8","-3","*7"]

# ages 16..120
age_low = 16.0
age_high = 120.0

# variables
m = makeIntVar(sol,"m",age_low, age_high) # my age
h = makeIntVar(sol,"h",age_low, age_high) # my husband's age

perm1 = makeIntVector(sol,"perm1",n,0,n-1)
perm2 = makeIntVector(sol,"perm2",n,0,n-1)

mlist = makeRealVector(sol,"mlist",n+1,0,1000.0) # for calculate my age 
hlist = makeRealVector(sol,"hlist",n+1,0,1000.0) # for calculate husbands age 

# constraints

sol.add(Distinct(perm1))
sol.add(Distinct(perm2))

# same operations in different order (though this is not really needed here)
sol.add(Sum([If(perm1[i] != perm2[i],1,0) for i in range(n)]) > 0)

      
# find husbands age, start with my age
sol.add(hlist[0] == m)

# husband's age is last in hlist
sol.add(h == hlist[n])

# checking my age, start with husband's age
sol.add(mlist[0] == h)

# my age is last in mlist
sol.add(m == mlist[n])

# check the operations
for i in range(n):
  check(sol,perm1[i], hlist[i], hlist[i+1])
  check(sol,perm2[i], mlist[i], mlist[i+1])

# Symmetry breaking: I'm younger than husband
sol.add(m < h)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("m:", mod[m], "h:", mod[h])
  print("hlist:", [mod[hlist[i]] for i in range(n+1)])
  print("mlist:", [mod[mlist[i]] for i in range(n+1)])
  print("perm1:", [perms[mod[perm1[i]].as_long()] for i in range(n)])
  print("perm2:", [perms[mod[perm2[i]].as_long()] for i in range(n)])
  print()
  getDifferentSolution(sol,mod,[m,h])

print("num_solutions:", num_solutions)

