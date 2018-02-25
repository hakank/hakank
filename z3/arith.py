#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint arith in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Carith.html
# """
# Enforce for all variables var of the VARIABLES collection to have 
#    var RELOP VALUE.
# 
# Example
# (<4, 5, 7, 4, 5>, <, 9)
# 
# The arith constraint holds since all values of the collection 
# <4, 5, 7, 4, 5> are strictly less than 9.
# """ 
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

relops = ["<","<=","=",">=",">","!="]

n = 5
x = makeIntVector(sol,"x", n, 0,4)
y = makeIntVar(sol,"y", 0,9)
relop = makeIntVar(sol,"relop", 0,len(relops)-1)

# constraints

sol.add(y <= 3)

arith(sol, x, relop, y)


num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print [mod[x[i]] for i in range(n)], relops[mod[relop].as_long()], mod[y]
    getDifferentSolution(sol,mod,x)

print "num_solutions:", num_solutions

