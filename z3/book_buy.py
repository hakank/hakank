#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Book buy puzzle in Z3
#
# From Martin Chlond Integer Programming Puzzles:
# http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 9.
# Source:  M Kraitchik, Mathematical Recreations(p37), Dover
#
# This model was inspired by the AMPL model created by Martin Chlond.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("QF_FD")

# fathers: 1 = Peter, 2 = Paul, 
# sons: 1 = Tom, 2 = Dick
m = 2

# w = 1 if Peter is Tom's father, 0 otherwise 
w = makeIntVar(sol,"m", 0,1)

# number of books (and price) bought by father i
x = makeIntVector(sol, "x", m, 1,8)
# number of books (and price) bought by son j
y = makeIntVector(sol, "y", m, 1,8)


sol.add(y[1] == 1)      # Dick buys one book
sol.add(x[0] == y[0]+1) # Peter buys one more book than Tom

# each family spends $65
sol.add(x[0]*x[0] + w*y[0]*y[0] + (1-w)*y[1]*y[1] == 65)
sol.add(x[1]*x[1] + (1-w)*y[0]*y[0] + w*y[1]*y[1] == 65)

while sol.check() == sat:
    mod = sol.model()
    ww = mod[w].as_long()
    print("w:", ww)
    print(["Peter","Paul"][ww], "is Dick's father")
    print("x:", [mod[x[i]] for i in range(m)])
    print("y:", [mod[y[i]] for i in range(m)])
    getDifferentSolution(sol,mod, x+y)
