#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Pigeon hole problem in Z3
#
# ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
# """
# pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes (at most 1
# pigeon per hole). The boolean formulation uses n × m variables to indicate, for each pigeon,
# its hole number. Obviously, there is a solution iff n <= m.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 3  # n pigeons
m = 10 # m pigeon holes

# variables
p = {}
for i in range(n):
  for j in range(m):
    p[(i,j)] = makeIntVar(sol,"p[%i,%i]" % (i,j),0,1)
print "p:",p

# max 1 pigeon per pigeon hole
for j in range(m):
  sol.add(Sum([p[(i,j)] for i in range(n)]) <= 1)

# all pigeon must be placed and only at one hole
for i in range(n):
  sol.add(Sum([p[(i,j)] for j in range(m)]) == 1)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  for i in range(n):
    for j in range(m):
      print mod.eval(p[(i,j)]),
    print
  print  
  getDifferentSolutionMatrix(sol,mod,p,n,m)

print "num_solutions:", num_solutions  
