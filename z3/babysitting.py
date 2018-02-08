#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Babysitting puzzle (Dell Logic Puzzles) in Z3
#
# Problem from http://brownbuffalo.sourceforge.net/BabysittingClues.html
# """
# Title: Babysitting
# Author: Scott Marley
# Publication: Dell Logic Puzzles
# Issue: April, 1998
# Page: 7
# Stars: 1
#
# Each weekday, Bonnie takes care of five of the neighbors' children. 
# The children's names are Keith, Libby, Margo, Nora, and Otto; last names 
# are Fell, Gant, Hall, Ivey, and Jule. Each is a different number of years 
# old, from two to six. Can you find each child's full name and age?
#
# 1. One child is named Libby Jule.
# 2. Keith is one year older than the Ivey child, who is one year older 
#    than Nora.
# 3. The Fell child is three years older than Margo.
# 4. Otto is twice as many years old as the Hall child.
#
# Determine: First name - Last name - Age 
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 5

first = range(n)
[Keith, Libby, Margo, Nora, Otto] = first

# variables
last = makeIntArray(sol,"last", n, 0, n-1)
Fell, Gant, Hall, Ivey, Jule = [last[i] for i in range(n)]

age = makeIntArray(sol, "age", n, 2, 6)

# constraints
sol.add(Distinct([last[i] for i in range(n)]))
sol.add(Distinct([age[i] for i in range(n)]))

#  1. One child is named Libby Jule.
sol.add(Jule == Libby)

#  2. Keith is one year older than the Ivey child, who is one 
#     year older than Nora.
sol.add(age[Keith] == age[Ivey] + 1)
sol.add(Keith != Ivey)

sol.add(age[Ivey] == age[Nora] + 1)
sol.add(Ivey != Nora)

#  3. The Fell child is three years older than Margo.
sol.add(age[Fell] == age[Margo] + 3)
sol.add(Fell != Margo )

#  4. Otto is twice as many years old as the Hall child.
sol.add(age[Otto] == age[Hall]*2)
sol.add(Otto != Hall)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "first:", [first[i] for i in range(n)]
  print "last :", [mod.eval(last[i]) for i in range(n)]
  print "age  :", [mod.eval(age[i]) for i in range(n)]
  print
  getDifferentSolution(sol,mod,[last[i] for i in range(n)],[age[i] for i in range(n)])

print "num_solutions:", num_solutions  
