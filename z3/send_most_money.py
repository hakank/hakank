#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# SEND + MOST = MONEY in z3
#
# Maximize the value of MONEY, and show all optimal solutions.
# 
# This z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *


def send_most_money():
    sol = Optimize()

    s,e,n,d,m,o,t,y,money = Ints("s e n d m o t y money")
    a = [s,e,n,d,m,o,t,y]
    alen = len(a)

    # domains
    for x in a:
        sol.add(x >= 0),
        sol.add(x <= 9)

    sol.add(Distinct(a))
    sol.add(10000*m + 1000*o + 100*n + 10*e + y == money)
    sol.add(1000*s + 100*e + 10*n + d  +  1000*m + 100*o + 10*s + t  == money )

    sol.add(s > 0)
    sol.add(m > 0)

    sol.maximize(money)

    if sol.check() == sat:
        # First get the optimal value of MONEY
        mod = sol.model()
        print("money=", mod.evaluate(money))
        print("All optimal solutions:")
        ss = evalArray(mod,a) # [mod.evaluate(x) for x in a]
        print(ss )
        sol.add(money == mod.evaluate(money))
        # At least some element must be different from the others
        getDifferentSolution(sol,mod,a)
        while sol.check() == sat:
            mod = sol.model()
            print(evalArray(mod, a)) # [mod.evaluate(x) for x in a]
            getDifferentSolution(sol,mod,a)
        
    # print(sol.statistics())

send_most_money()
