#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Mama' age problem in Z3.
# 
# Mamma's Age from "Amusements in Mathematics, Dudeney", number 40.
# """
# Tommy: "How old are you, mamma?"
# Mamma: "Our three ages add up to exactly seventy years."
# Tommy: "And how old are you, papa?"
# Papa: "Just six times as old as you, my son."
# Tommy: "Shall I ever be half as old as you, papa?"
# Papa: "Yes, Tommy; and when that happens our three ages will add up to
# exactly twice as much as today."
#
# Can you find the age of Mamma?
# """

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

m = makeIntVar(sol,"m", 1,500) # mamma's age
p = makeIntVar(sol,"p", 1,500) # papa's age
t = makeIntVar(sol,"t", 1,500) # tommy's age
i = makeIntVar(sol,"i", 1,500); # temp

# the real ages
m2 = makeIntVar(sol,"m2", 1,100); # mamma's age
p2 = makeIntVar(sol,"p2", 1,100); # papa's age
t2 = makeIntVar(sol,"t2", 1,100); # tommy's age

sol.add(
   m + p + t == 70 * 12,
   6 * t == p,
   (t + i) * 2 == p + i,
   m + i + p + i + t + i == 2 * (m + p + t)
  ,
   m2 == m / 12,
   p2 == p / 12,
   t2 == t / 12
   )

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "m2:", mod.eval(m2)
  print "p2 :", mod.eval(p2)
  print "t2  :",mod.eval(t2)
  print
  getDifferentSolution(sol,mod,[m,p,t,i,m2,p2,t2])

print "num_solutions:", num_solutions  

