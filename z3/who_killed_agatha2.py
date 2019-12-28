#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Who killed agatha? (The Dreadsbury Mansion Murder Mystery), "pure logic" approach in Z3
#
# http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
# 
# """ 
# Someone in Dreadsbury Mansion killed Aunt Agatha. 
# Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
# are the only ones to live there. A killer always hates, and is no 
# richer than his victim. Charles hates noone that Agatha hates. Agatha 
# hates everybody except the butler. The butler hates everyone not richer 
# than Aunt Agatha. The butler hates everyone whom Agatha hates. 
# Noone hates everyone. Who killed Agatha? 
# """
#
# Originally from 
# F. J. Pelletier: Seventy-five problems for testing automatic theorem provers.
# Journal of Automated Reasoning, 2: 191-216, 1986.
#
# As usual, this model yields 8 solutions, all indicating that Agatha is the killer.
# For a detailed discussion on this, see
# http://www.hakank.org/minizinc/who_killed_agatha_dmcommunity_challenge.html
#
#
# This is a "pure logic" approach using EnumSort, and yield just one solution:
# Agatha killed Agatha.
#
# (Compare with the constraint model http://hakank.org/z3/who_killed_agatha.py
# which yield 8 solutions, all stating that Agatha killed herself.)
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

set_param(proof=True)
sol = Solver()

# Variables

# Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
# are the only ones to live there. 
People, (agatha, butler, charles) = EnumSort("People",["agatha","butler","charles"])

# Who hates whom?
hates = Function("hates", People, People, BoolSort())
# Who is richer than whom?
richer = Function("richer", People, People, BoolSort())
# Who killed agatha?
killer = Function("killer", People, People, BoolSort())

# For the logics
x = Const("x", People)
y = Const("y", People)

# constraints

# Who killed Agatha?
sol.add(Exists([x], killer(x, agatha)))

# A killer always hates, and is no richer than his victim. 
sol.add(ForAll([x,y], Implies(killer(x,y), And(hates(x,y), Not(richer (x,y))))))

# Charles hates noone that Agatha hates. 
sol.add(ForAll([x], Implies(hates(agatha, x), Not(hates(charles, x)))))

# Agatha hates everybody except the butler.        
sol.add(hates(agatha, agatha), hates(agatha, charles))

# The butler hates everyone not richer than Aunt Agatha. 
sol.add(ForAll([x], Implies(Not(richer(x,agatha)), hates(butler, x))))

# The butler hates everyone whom Agatha hates. 
sol.add(ForAll([x], Implies(hates(agatha,x), hates(butler,x))))

# Noone hates everyone. 
sol.add(ForAll([x], Exists ([y], Not(hates(x, y)))))

sol.add(Not(killer(agatha,agatha)))

num_solutions = 0
# print(sol)
ll = [agatha,butler,charles]
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    # print(mod)
    print("Richer: agatha,butler,charles\n",end=" ")
    for i in ll:
        print(i, ":",end=" ")
        for j in ll:
            print(mod.eval(richer(i,j)),end=" ")
        print()
    print()
    print("Hates: agatha,butler,charles\n",end=" ")
    for i in ll:
        print(i, ":", end=" ")
        for j in ll:
            print(mod.eval(hates(i,j)),end=" ")
        print()
    print()
    for i in ll:
        print(i, "killed agatha", mod.eval(killer(i,agatha)))
    print()
    print("killer:", mod.eval(killer(x,agatha)))
    print()
    sol.add(x != mod.eval(x))

if sol.check() == unsat:
  for pp in sol.proof().children():
      print(pp)
      print()

print("num_solutions:", num_solutions)
