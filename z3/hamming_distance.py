#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Hamming distance in Z3
#
# I.e. the number of bits differing in two (binary) arrays.
# See http://en.wikipedia.org/wiki/Hamming_distance
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

def hamming_distance(sol, a, b, d):
  sol.add(d == Sum([If(a[i] != b[i],1,0) for i in range(len(a))]))


n = 6 # length of the arrays
a_given = [1,1,1,1,0,0]

a = makeIntVector(sol,"a",n,0,1)
b = makeIntVector(sol,"b",n,0,1)
diffs = makeIntVar(sol,"diffs",0,n) # The number of differences 

# for i in range(n):
#     sol.add(a[i] == a_given[i])

#
# We can now either
# - Calculate the hamming distance from two arrays
# - Given the distance, generate all arrays which has the hamming distance
#

hamming_distance(sol, a, b, diffs)
# sol.add(diffs == 2)


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("a:", evalArray(mod,a))
  print("b:", evalArray(mod,b))
  print("diffs:", mod.eval(diffs))
  print()
  getDifferentSolution(sol,mod, a, b)

print("num_solutions:", num_solutions)

