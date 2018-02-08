#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Exodus puzzle (Dell Logic Puzzles) in Z3
#
# From 
# http://brownbuffalo.sourceforge.net/ExodusClues.html
# """
# Title: Exodus
# Author: Sophy McHannot
# Publication: Dell Logic Puzzles
# Issue: April, 1998
# Page: 14
# Stars: 2

# In preparation for Passover, five children at Hebrew school 
# (Bernice,Carl,Debby,Sammy, and Ted) 
# have been chosen to present
# different parts of the story of the Exodus from Egypt 
#  (burning bush, captivity,
#   Moses's youth, Passover, or the Ten Commandments). 
# Each child is a different age 
#   (three, five, seven, eight, or ten), 
# and the family of each child has recently made its own exodus 
# to America from a different country 
# (Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
# Can you find the age of each child, his or her family's country of 
# origin, and the part of the Exodus story each related?

#  1. Debby's family is from Lithuania.
#  2. The child who told the story of the Passover is two years older
#     than Bernice.
#  3. The child whose family is from Yemen is younger than the child from
#     the Ethiopian family.
#  4. The child from the Moroccan family is three years older than Ted.
#  5. Sammy is three years older than the child who told the story of
#     Moses's youth in the house of the Pharaoh.
#  6. Carl related the story of the captivity of the Israelites in Egypt.
#  7. The five-year-old child told the story of the Ten Commandments.
#  8. The child who told the story of the burning bush is either two or
#     three years older than the one whose family came from
#     Kazakhstan.
#
# Determine: Age -- Child -- Country -- Story
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 5

people = range(n)
[Bernice,Carl,Debby,Sammy,Ted] = people

# variables
Story = makeIntArray(sol,"Story",n, 0,n-1)
BurningBush, Captivity, MosessYouth, Passover, TenCommandments = [Story[i] for i in range(n)]

vals = [3,5,7,8,10]
Age = makeIntArray(sol, "Age",n, min(vals),max(vals))
for i in range(n):
    sol.add(Or([Age[i] == v for v in vals]))

Country = makeIntArray(sol,"Country",n, 0,n-1)
[Ethiopia, Kazakhstan, Lithuania, Morocco, Yemen] = [Country[i] for i in range(n)]

# constraints
sol.add(Distinct([Story[i] for i in range(n)]))
sol.add(Distinct([Age[i] for i in range(n)]))
sol.add(Distinct([Country[i] for i in range(n)]))

sol.add(Debby == Lithuania)
sol.add(Age[Passover] == Age[Bernice] + 2)
sol.add(Age[Yemen] < Age[Ethiopia])
sol.add(Age[Morocco] == Age[Ted] + 3)
sol.add(Age[Sammy] == Age[MosessYouth] + 3)
sol.add(Carl == Captivity)
sol.add(Age[TenCommandments] == 5)
sol.add( 
       Or(Age[BurningBush] == Age[Kazakhstan] + 2
       ,
       Age[BurningBush] == Age[Kazakhstan] + 3
       )
      )

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "People :", [people[i] for i in range(n)]
  print "Story  :", [mod.eval(Story[i]) for i in range(n)]
  print "Country:", [mod.eval(Country[i]) for i in range(n)]
  print "Age    :", [mod.eval(Age[i]) for i in range(n)]  
  print
  getDifferentSolution(sol,mod,
                       [Story[i] for i in range(n)],
                       [Age[i] for i in range(n)],
                       [Country[i] for i in range(n)])

print "num_solutions:", num_solutions  





