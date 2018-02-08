#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Twin letters problem in Z3
#
# From
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
# """
# Twin Letters    
#
# In the following puzzle, there are ten pairs of
# letters to be assigned to the same digit so that the multiplication
# (including intermediate results) is correct. Can you find out the
# pairs and their values?
#
#         A B C
#  *      D E F
#  ____________
#         G H I
#       J K L
#     M N O
#  ____________
#     P Q R S T
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

# sol = Solver() # 18.1s
sol = SolverFor("QF_LIA") # 11.7s

n = 20

# variables
x = makeIntVector(sol,"x",n, 0,9)
[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = x

C1 = makeIntVar(sol,"C1",0,1)
C2 = makeIntVar(sol,"C2",0,2)
C3 = makeIntVar(sol,"C3",0,1)

# constraints

# exact 2 occurrences of each digit
for i in range(9+1):
  sol.add(count2(sol, i, x) == 2)

sol.add(
             100*G + 10*H + I +
    1000*J + 100*K + 10*L +
    10000*M + 1000*N + 100*O ==
    10000*P + 1000*Q + 100*R + 10*S + T
     )
    
sol.add((100*D + 10*E + F)*C == 100*G + 10*H + I)
sol.add((100*D + 10*E + F)*B == 100*J + 10*K + L)
sol.add((100*D + 10*E + F)*A == 100*M + 10*N + O)
    
sol.add((100*A + 10*B + C) * (100*D + 10*E + F) ==
    10000*P + 1000*Q + 100*R + 10*S + T)

#  carry restrictions
sol.add(T == I)
sol.add(S + 10*C1 == H + L)
sol.add(R + 10*C2 == G + K + O + C1)
sol.add(Q + 10*C3 == J + N + C2)
sol.add(P         == M + C3)


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "x:", [mod.eval(x[i]) for i in range(n)]
  print "C1 :", mod.eval(C1)
  print "C2 :", mod.eval(C2)
  print "C3 :", mod.eval(C3)
  print
  getDifferentSolution(sol,mod,[x[i] for i in range(n)],[C1,C2,C3])

print "num_solutions:", num_solutions  
