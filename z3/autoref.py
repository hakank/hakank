#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Autoref problem in Z3
#
# From Global constraint catalog
# http://www.emn.fr/z-info/sdemasse/gccat/Kautoref.html
# """
# A constraint that allows for modelling the autoref problem with one single constraint. 
# The autoref problem is a generalisation of the problem of finding a magic serie 
# and can be defined in the following way. Given an integer n > 0 and an integer 
# m >= 0, the problem is to find a non-empty finite series S=(s0,s1,...,sn,sn+1) 
# such that (1) there are si occurrences of i in S for each integer i ranging 
# from 0 to n, and (2) sn+1=m. This leads to the following model:
# 
# global_cardinality(
#  <var-s0,var-s1,...,var-sn,var-m>,
#  val-0 noccurrence-s0
#  val-1 noccurrence-s1,
#  < ... >
#  val-n noccurrence-sn
# )
# 
#
# 23, 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 5 
# and 
# 23, 3, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 5 
# are the two unique solutions for n=27 and m=5.
# """
#  
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


# creates an Array with a domain
def makeByteArray(sol,name, bitVec, size, min_val, max_val):
    a = [BitVec("%s_%i"% (name,i),bitVec) for i in range(size) ]
    return a

sol = SolverFor("QF_FD")

n = 27
m = 5

# variables

s = [Int(f"s[{i}") for i in range(n+2)]
for i in range(n+2):
    sol.add(s[i] >= 0, s[i] <= n)

# constraints

sol.add(s[n+1]==m)
for i in range(n+1):
    sol.add(s[i] == Sum([If(s[j] == i,1,0) for j in range(n+2)]))    

num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print([mod.eval(s[i]) for i in range(n+2)])
    getDifferentSolution(sol,mod,[s[i] for i in range(n+2)])

print("num_solutions:", num_solutions)



 
