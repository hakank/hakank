#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# General store problem in Z3
#
# From
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
#
# """
# The General Store  from "Mathematical Puzzles of Sam Loyd", number 30
#
# The owner of a general store, who is something of a puzzlist, has put
# up this sign to see if any of his mathematical friends can translate
# it properly. Each different letter stands for a different digit. The
# words above the horizontal line represent numbers that add to the
# total of "ALL WOOL". The problem is to change all the letters to the
# correct digits.
#
#        C H E S S
#  +       C A S H
#  +   B O W W O W
#  +     C H O P S
#  +   A L S O P S
#  + P A L E A L E
#  +       C O O L
#  +       B A S S
#  +       H O P S
#  +       A L E S
#  +       H O E S
#  +   A P P L E S
#  +       C O W S 
#  +   C H E E S E
#  +   C H S O A P
#  +     S H E E P
#  _______________
#    A L L W O O L
#  """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

# sol = Solver() # Very slow ~24minutes
sol = SolverFor("QF_NIA") # 5.5s
# sol = SolverFor("NIA") # slow
# sol = SolverFor("AUFLIA") # 1min 26s
# sol = SolverFor("AUFNIA") # long time


n = 10

# variables
x = makeIntVector(sol,"x",n,0,9)
C, H, E, S, A, B, O, W, P, L = x

# constraints
sol.add(Distinct(x))

sol.add(                              10000*C + 1000*H + 100*E + 10*S + S
   +                                  1000*C + 100*A + 10*S + H
   +             100000*B + 10000*O + 1000*W + 100*W + 10*O + W
   +                        10000*C + 1000*H + 100*O + 10*P + S
   +             100000*A + 10000*L + 1000*S + 100*O + 10*P + S
   + 1000000*P + 100000*A + 10000*L + 1000*E + 100*A + 10*L + E
   +                                  1000*C + 100*O + 10*O + L
   +                                  1000*B + 100*A + 10*S + S
   +                                  1000*H + 100*O + 10*P + S
   +                                  1000*A + 100*L + 10*E + S
   +                                  1000*H + 100*O + 10*E + S
   +             100000*A + 10000*P + 1000*P + 100*L + 10*E + S
   +                                  1000*C + 100*O + 10*W + S
   +             100000*C + 10000*H + 1000*E + 100*E + 10*S + E
   +             100000*C + 10000*H + 1000*S + 100*O + 10*A + P
   +                        10000*S + 1000*H + 100*E + 10*E + P   
  == 1000000*A + 100000*L + 10000*L + 1000*W + 100*O + 10*O + L
)

if sol.check() == sat:
  mod = sol.model()
  print("x:", [mod.eval(x[i]) for i in range(n)])
