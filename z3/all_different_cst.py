#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint all_different_cst in Z3
#
# From Global Constraint Catalog:
# http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_cst.html
# """
# For all pairs of items (VARIABLES[i], VARIABLES[j]) (i!=j) of the 
# collection VARIABLES enforce 
# VARIABLES[i].var+VARIABLES[i].cst != VARIABLES[j].var+VARIABLES[j].cst.
#
# Example
#  (<
#     var-5 cst-0,
#     var-1 cst-1,
#     var-9 cst-0,
#     var-3 cst-4
#  >
#  )
#
# The alldifferent_cst constraint holds since all the expressions 
# 5+0=5, 1+1=2, 9+0=9 and 3+4=7 correspond to distinct values.
# """  

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

# data

n = 4
cst = [0,1,0,4] # the cst shown above
# cst = [0,0,0,0] # for plain all_different

x = makeIntVector(sol,"x",n,1,9)


# constraints

all_different_cst(sol,x,cst)

num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx = [mod[x[i]].as_long() for i in range(n)]
    print "x:", xx, "cst:", cst, "y:", [xx[i] + cst[i] for i in range(n)]
    getDifferentSolution(sol,mod,x)

print "num_solutions:", num_solutions
