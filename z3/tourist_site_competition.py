#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Tourist site competition in Z3
#
# From Pierre Flener's presentation 
# "Constraint Technology - A Programming Paradigm on the Rise"
# http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
#    pages 5f: problem statement 
#    pages 12f: model
#    pages 21ff: walktrough of a solution
#
# With 7 tourist sites and 7 judges:
# """
# Every tourist site is visited by r = 3 judges.
# Every judge visits c = 3 tourist sites.
# Every pair of sites is visited by lambda = 1 common judge.
# """
#
# There are 151200 solutions to this problem.
# With the additional constraint that Ali should visit Birka, Falun and Lund
# there are 4320 solutions.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

r = 3
c = 3
llambda = 1

# sites
num_sites = 7
sites = range(num_sites)
[Birka,Falun,Lund,Mora,Sigtuna,Uppsala,Ystad] = sites

sites_s = ["Birka","Falun","Lund","Mora","Sigtuna","Uppsala","Ystad"]

# judges
num_judges = 7
judges = range(num_judges)
[Ali,Dan,Eva,Jim,Leo,Mia,Ulla] = judges
judges_s = ["Ali","Dan","Eva","Jim","Leo","Mia","Ulla"]

# variables
x = {}
for s in range(num_sites):
  for j in range(num_judges):
      x[(s,j)] = makeIntVar(sol,"x[%i,%i]" % (s,j),0,1)

# judges_where = makeIntVector(sol,"judges_where",num_judges,1,num_judges)

# symmetry breaking
for s in range(3):
    sol.add(x[(s, 0)] == 1)

# Every tourist site is visited by r judges.
for s in range(num_sites):
  sol.add(r == Sum([x[(s,j)] for j in range(num_judges)]))

# Every judge visits c tourist sites.
for j in range(num_judges):
  sol.add(c == Sum([x[(s,j)] for s in range(num_sites)]))

# Every pair of sites is visited by lambda common judge.
for s1 in range(num_sites):
  for s2 in range(num_sites):
    if s1 < s2:
      sol.add(llambda == Sum([If(And(x[(s1,j)] == 1,x[(s1,j)] == x[s2,j]),1,0) for j in range(num_judges)]))

# where are the judges?
# for j in range(num_judges):
#   for s in range(num_sites):
#     sol.add((x[s,j] == 1) == member_of(sol,s,judges_where[j]))

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  xx = [ [mod.eval(x[s,j]).as_long() for j in range(num_judges) ] for s in range(num_sites)]
  # for s in range(num_sites):
  #   for j in range(num_judges):
  #     print(xx[s][j],end=" ")
  #   print()
  # print()
  # where are the judges
  for j in range(num_judges):
    print(judges_s[j], ":",end=" ")
    for s in range(num_sites):
      if xx[s][j] == 1:
        print(sites_s[s],end=" ")
    print()
  print()
  getDifferentSolutionMatrix(sol,mod,x,num_sites,num_judges)

print("num_solutions:", num_solutions)
                     





