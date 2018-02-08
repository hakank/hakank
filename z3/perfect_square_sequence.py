#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Perfect square sequence in Z3
#
# From "Fun with num3ers"
# "Sequence"
# http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
# """
# If we take the numbers from 1 to 15 
#     (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
# and rearrange them in such an order that any two consecutive 
# numbers in the sequence add up to a perfect square, we get,
# 
# 8     1     15     10     6     3     13     12      4      5     11     14   #    2      7      9
#     9    16    25     16     9     16     25     16     9     16     25     16#      9     16
# 
# 
# I ask the readers the following:
# 
# Can you take the numbers from 1 to 25 to produce such an arrangement?
# How about the numbers from 1 to 100?
# """
# 
# Via http://wildaboutmath.com/2010/11/26/wild-about-math-bloggers-111910
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def perfect_square_sequence(n=15):
    
    sol = SolverFor("NIA")
    squares = [i*i for i in range(int(math.sqrt(n*n)))]

    # variables
    x = makeIntVector(sol,"x", n, 1, n)

    # constraints
    sol.add(Distinct(x))
    for i in range(1,n):
      member_of(sol, x[i-1]+x[i], squares)

    # symmetry breaking
    sol.add(x[0] < x[n-1])

    if sol.check() == sat:
      mod = sol.model()
      print n,":", [mod.eval(x[i]) for i in range(n)]
      getDifferentSolution(sol,mod,x)
    else:
      print n, ":", "No solution"

    # print "num_solutions:", num_solutions  


for i in range(1,50):
  perfect_square_sequence(i)
