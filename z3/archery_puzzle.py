#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Archery puzzle in Z3
#
# http://www.eonhq.com/m/puzzles/images/archery-puzzle.jpg
# Archery puzzle by Sam Loyd:
# """
# How close can the young archer come to scoring a total of
# 100 - using as many arrows as she please.
# [The targets are: 16, 17, 23, 24, 39, 40]
# """
# Via: The Aperiodical: "Manchester MathsJam June 2012 Recap"
# http://aperiodical.com/2012/06/manchester-mathsjam-june-2012-recap/
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


sol = SimpleSolver()

targets = [16, 17, 23, 24, 39, 40]
n = len(targets)

target = 100

# variables

x = makeIntVector(sol,"x",n,0,10)
z = Int("z")
d = Int("d")

# constraints
sol.add(z == Sum([x[i]*targets[i] for i in range(n)]))
sol.add(d == Abs(target-z))


num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("z:", mod[z])
    print("d:", mod[d])
    xx = [mod[x[i]].as_long() for i in range(n)]
    print("\n".join(["%i hits on score %i" % (xx[i], targets[i]) for i in range(n) if xx[i] > 0]))
    print()
    getLessSolution(sol,mod,d)

print("num_solutions:", num_solutions)


