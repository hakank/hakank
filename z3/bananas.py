#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Bananas problem in Z3
#
# """
# In three dollars, you get 5 bananas, in five dollars, 7 oranges, in
# seven dollars, 9 mangoes and in nine dollars, three apples, I need to
# purchase 100 fruits in 100 dollars. Please keep in mind that all type
# of fruits need to be purchased but I do not like banana and apple, so
# these should be of minimum quantity.
# """
# Note: I have forgot where I got this problem.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

# sol = Optimize()
sol = SolverFor("QF_LIA")

bananas = Int("bananas")
oranges = Int("oranges")
mangoes = Int("mangoes")
apples = Int("apples")
the_sum = Int("the_sum")

sol.add(bananas >= 1,
      oranges >= 1,
      mangoes >= 1,
      apples >= 1,
      the_sum == bananas+apples,
      3*bananas/5 + 5*oranges/7 + 7*mangoes/9 + 9*apples/3 == 100,
      bananas + oranges + mangoes + apples == 100)

# sol.minimize(the_sum)

while sol.check() == sat:
    mod = sol.model()
    print("the_sum:", mod[the_sum])
    print("bananas:", mod[bananas])
    print("oranges:", mod[oranges])
    print("mangoes:", mod[mangoes])
    print("apples :", mod[apples])
    print()
    getLessSolution(sol,mod,the_sum)
