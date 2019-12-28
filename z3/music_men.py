#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Music men puzzle in Z3
#
# From
## http://groups.google.com/groups?q=FINITE+DOMAINS+With+Logic+Puzzles&hl=en&lr=&ie=UTF-8&c2coff=1&safe=off&selm=1992Jul27.034607.19386#40IRO.UMontreal.CA&rnum=4
# """"
# MUSIC MEN
#
# Three friends like different kinds of music.  From the clues given
# below, can you identify them, say how old each is, and work out
# his musical preference?
#
# Clues: 
# 1.      Rob is older than Queen, who likes classical music.
# 2.      The pop-music fan, who is not Prince, is not 24.
# 3.      Leon, who is not King, is 25.
# 4.      Mark's musical preference is not jazz.

# Knowledge: "this is what we know of the world."
# Names           : Leon, Mark, Rob.
# Surnames        : King, Prince, Queen.
# Ages            : 24, 25, 26.
# Music           : Classical, Jazz, Pop.
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 3
min_age = 24
max_age = 26

# variables
Age24 = 24
Age25 = 25
Age26 = 26
Age  = [Age24, Age25, Age26];
Names  = makeIntVector(sol,"Names", n, min_age,max_age)

[King, Prince, Queen] = Names
Surnames = makeIntVector(sol,"Surnames", n, min_age,max_age)
[Leon, Mark, Rob] = Surnames
Music    = makeIntVector(sol,"Music", n, min_age,max_age)
[Classical, Jazz, Pop] = Music

# constraints

sol.add(Distinct(Names))
sol.add(Distinct(Surnames))
sol.add(Distinct(Music))

# Rob is older than Queen  who likes classical music.
sol.add(Rob > Queen )
sol.add(Queen == Classical)

# The pop-music fan  who is not Prince  is not 24.
sol.add(Pop != Prince)
sol.add(Pop != Age24)

# Leon  who is not King  is 25.
sol.add(Leon != King)
sol.add(Leon == Age25)

#  Mark's musical preference is not jazz.
sol.add(Mark != Jazz)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("Names   :", [mod.eval(Names[i]) for i in range(n)])
  print("Surnames:", [mod.eval(Surnames[i]) for i in range(n)])
  print("Music   :", [mod.eval(Music[i]) for i in range(n)])
  print()
  getDifferentSolution(sol,mod,Names,Surnames,Music)

print("num_solutions:", num_solutions)

