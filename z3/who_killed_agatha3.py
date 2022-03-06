#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in Z3
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
# This is version is an alternative to who_killed_agatha.py:
# it use Function instead of Array for the two matrices hates and richer.
# This makes it possible to use a "natural" matrix element constraint such as:
#    hates(the_killer,the_victim)  == True
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SimpleSolver()

n = 3
agatha = 0
butler = 1
charles = 2

who = ["agatha","butler","charles"]

the_killer = Int("the_killer")
sol.add(the_killer >= agatha, the_killer <= charles)
the_victim = agatha

hates = Function("hates", IntSort(), IntSort(), BoolSort())
richer = Function("richer", IntSort(), IntSort(), BoolSort())

# Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
# are the only ones to live there. 
sol.add(hates(the_killer,the_victim)  == True)

# A killer always hates, and is no richer than his victim. 
sol.add(hates(the_killer,the_victim) == True)
sol.add(richer(the_killer,the_victim) == False)

# # define the concept of richer: no one is richer than him-/herself
for i in range(n):
    sol.add(richer(i,i) == False)

# (contd...) if i is richer than j then j is not richer than i
for i in range(n):
    for j in range(n):
        if i != j:
            sol.add( (richer(i,j) == True) == (richer(j,i) == False))

# Charles hates noone that Agatha hates. 
for i in range(n):
    sol.add(Implies(hates(agatha,i) == True, hates(charles,i) == False))


# Agatha hates everybody except the butler.
sol.add(hates(agatha,charles) == True)
sol.add(hates(agatha,agatha) == True)
sol.add(hates(agatha,butler) == False)

# The butler hates everyone not richer than Aunt Agatha. 
for i in range(n):
    sol.add(Implies(richer(i,agatha) == False, hates(butler,i) == True ))

# The butler hates everyone whom Agatha hates. 
for i in range(n):
    sol.add(Implies(hates(agatha,i) == True, hates(butler,i) == True ))

# Noone hates everyone. 
for i in range(n):
    sol.add(Sum([If(hates(i,j) == True,1,0) for j in range(n)]) <= 2)

# Who killed Agatha? 

num_solutions = 0
while sol.check() == sat:
    mod = sol.model()
    # print(mod)
    num_solutions += 1
    killer = mod.eval(the_killer)
    print("the_killer:", who[killer.as_long()])
    sol.add(
        Or(killer != the_killer,
           Or([hates(i,j) != mod.eval(hates(i,j)) for i in range(n) for j in range(n)]),
           Or([richer(i,j) != mod.eval(richer(i,j)) for i in range(n) for j in range(n)])
          )
        )
        
print("num_solutions:", num_solutions)
