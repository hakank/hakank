#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Mortgage in Z3
# 
# Marriot & Stuckey: Programming with Constraints, page 175f
# (famous early example)
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def mortgage(T, I1,R1,P1, max_val):
    
    sol = Solver()

    I = makeRealVar(sol, "I", 0.0,max_val)
    R = makeRealVar(sol, "R", 0.0,max_val)
    P = makeRealVar(sol, "P", 0.0,max_val)

    min_var = None
    
    if I1 != None:
      sol.add(I == I1)
    else:
      min_var = I

    if R1 != None:
      sol.add(R == R1)
    else:
      min_var = R

    if P1 != None:
       sol.add(P == P1)
    else:
      min_var = P

    mortgage = makeRealVector(sol,"mortgage", T, 0,T*max_val)

    for i in range(T):
      sol.add(mortgage[i] >= 0.0)

    # start value:
    sol.add(mortgage[0] == P + (P  * I) - R)

    for i in range(1,T):
      # calculate the next value
      sol.add(mortgage[i] == mortgage[i-1] + (mortgage[i-1]  * I) - R)

    # sol.minimize(P)
    num_solutions = 0
    last_min_var = -1
    while sol.check() == sat:
      num_solutions += 1
      mod = sol.model()
      print "mortgage :", [mod.eval(mortgage[i]).as_decimal(16) for i in range(T)]
      print "min_var: ", min_var
      print "I  :", mod.eval(I).as_decimal(16)
      print "R  :", mod.eval(R).as_decimal(16)
      print "P  :", mod.eval(P).as_decimal(16)
      print
      # Since Z3 has infinite precision we must decide when to stop
      if last_min_var == mod.eval(min_var).as_decimal(16):
        break
      else:
        getLessSolution(sol,mod,min_var)
      last_min_var = mod.eval(min_var).as_decimal(16)  

      # getLessSolution(sol,mod,mortgage[T-1])      

    print "num_solutions:", num_solutions  


# comment one of the initiations to calculate it:
I = None
R = None
P = None
I = 10.0/100.0
# R = 150.0
P = 373.02779
max_val = 10000
T = 6 # time period
mortgage(T,I,R,P, max_val)
