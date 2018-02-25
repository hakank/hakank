#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint all_equal in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Call_equal.html
# """
# Constraint
#
#     all_equal(VARIABLES)
#
# Purpose
#
#     Enforce all variables of the collection VARIABLES to take the same value.
#
# Example
#     (<5, 5, 5, 5>)
#
# The all_equal constraint holds since all its variables are fixed to value 5.
# """

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

def all_equal(sol,x):
    sol.add(And([x[i] == x[i-1] for i in range(len(x))]))
        

sol = Solver()

n = 4

# variables
x = makeIntVector(sol,"x",n,0,6)

# constraints
all_equal(sol,x)


num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print [mod[x[i]] for i in range(n)]
    getDifferentSolution(sol,mod,x)
    

print "num_solutions:", num_solutions

