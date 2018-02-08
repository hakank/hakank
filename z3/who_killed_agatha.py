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
#
# Note: This model is slighly awkward since there's no matrix construct in Z3
# that also supports the element constraint, so we have to use the linear approach,
# i.e. y[i*n-j], instead of y[i,j]
# 
#

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 3
agatha = 0
butler = 1
charles = 2

who = ["agatha","butler","charles"]

the_killer = Int("the_killer")
sol.add(the_killer >= agatha, the_killer <= charles)
the_victim = agatha

hates = Array("hates", IntSort(), IntSort())
richer = Array("richer", IntSort(), IntSort())
for i in range(n*n):
    sol.add(hates[i] >= 0, hates[i] <= 1)
    sol.add(richer[i] >= 0, richer[i]<= 1)

# Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
# are the only ones to live there. 
sol.add(hates[the_killer*n+the_victim]  == 1)


# A killer always hates, and is no richer than his victim. 
sol.add(hates[the_killer*n + the_victim] == 1)
sol.add(richer[the_killer*n + the_victim] == 0)

# # define the concept of richer: no one is richer than him-/herself
for i in range(n):
    sol.add(richer[i*n + i] == 0)

# (contd...) if i is richer than j then j is not richer than i
for i in range(n):
    for j in range(n):
        if i != j:
            sol.add( (richer[i*n+j] == 1) == (richer[j*n+i] == 0))

# Charles hates noone that Agatha hates. 
for i in range(n):
    sol.add(Implies(hates[agatha*n+i] == 1, hates[charles*n+i] == 0 ))


# Agatha hates everybody except the butler.
sol.add(hates[agatha*n+charles] == 1)
sol.add(hates[agatha*n+agatha] == 1)
sol.add(hates[agatha*n+butler] == 0)

# The butler hates everyone not richer than Aunt Agatha. 
for i in range(n):
    sol.add(Implies(richer[i*n+agatha] == 0, hates[butler*n+i] == 1 ))

# The butler hates everyone whom Agatha hates. 
for i in range(n):
    sol.add(Implies(hates[agatha*n+i] == 1, hates[butler*n+i] == 1 ))

# Noone hates everyone. 
for i in range(n):
    sol.add(Sum([hates[i*n+j] for j in range(n)]) <= 2)

# Who killed Agatha? 

num_solutions = 0
while sol.check() == sat:
    mod = sol.model()
    num_solutions += 1
    killer = mod.eval(the_killer)
    print "the_killer:", who[killer.as_long()]
    h = [mod.eval(hates[i]) for i in range(n*n)]
    # print "hates: ", h
    r = [mod.eval(richer[i]) for i in range(n*n)]
    # print "richer:", r
    sol.add(
        Or(killer != the_killer,
           Or([hates[i] != h[i] for i in range(n*n)]),
           Or([richer[i] != r[i] for i in range(n*n)])
          )
        )
        
print "num_solutions:", num_solutions
