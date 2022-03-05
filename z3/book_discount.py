#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Discount puzzle in Z3
#
# From Martin Chlond Integer Programming Puzzles:
# http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 11.
# Source:  J. & L. Poniachik, Hard-to-Solve Brainteasers (p16), Sterling
# """
# 11. Nice Discounts
# A bookstore has a nice discount policy. If you buy a $20 book today, you get a 2% discount 
# on your next purchase. If you buy a $15 book, you get a 1.5% discount on your next purchase. 
# That is, for each $10 you spend you get 1% discount on your next purchase. If you have to 
# buy three books that cost $10, $20 and $30, you could buy the $30 book today, the $10 book 
# tomorrow (on which you'll get a 3% discount), and the $20 book the following day (on which 
# you'll get a 1% discount). Or you could buy the $30 book and the $20 book today, and the 
# $10 book tomorrow (with a 5% discount). What is the cheapest way to buy five books priced at
# $10, $20, $30, $40 and $50? 
#
# (Poniachek)
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("QF_LRA")

cost = [10, 20, 30, 40, 50] # cost of books
m = len(cost)

# variables

# x[i,j]=1 if book i bought on day j, 0 otherwise
x = {}
for i in range(m):
    for j in range(m):
        x[i,j] = makeIntVar(sol,"x[%i,%i]"%(i,j),0,1)
        
# total cost of books on day j        
t = makeRealVector(sol,"t", m, 0,100) 

tot_cost = Real("tot_cost") # makeRealVar(sol,"tot_cost",0.0,200.0)

# constraints

sol.add(tot_cost == Sum([cost[i] for i in range(m)]) - sum([0.001*t[k-1]*t[k] for k in range(1,m)]))

for j in range(m):
    sol.add(t[j] == Sum([ cost[i]*x[i,j] for i in range(m)]))

for i in range(m):
    sol.add(Sum([x[i,j] for j in range(m)]) == 1)


num_solutions = 0
while sol.check() == sat:
    mod = sol.model()
    print("tot-cost:", mod[tot_cost].as_decimal(5))
    print("t:", [mod[t[i]] for i in range(m)])
    print("x:")
    for i in range(m):
        for j in range(m):
            print(mod[x[i,j]],end=" ")
        print()
    print()
    getLessSolution(sol,mod,tot_cost)

