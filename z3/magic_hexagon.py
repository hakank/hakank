#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Magic hexagon in Z3
#
# Prob023: Magic Hexagon
# http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
# http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

N = 19

[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s] = makeIntArrayVector(sol,"x",N,1,N)
LD = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]

sol.add(Distinct(LD))
sol.add(a + b + c ==  38)
sol.add(d + e + f + g ==  38)
sol.add(h + i + j + k + l ==  38)
sol.add(m + n + o + p ==  38)
sol.add(q + r + s ==  38)
sol.add(a + d + h ==  38)
sol.add(b + e + i + m ==  38)
sol.add(c + f + j + n + q ==  38)
sol.add(g + k + o + r ==  38)
sol.add(l + p + s ==  38)
sol.add(c + g + l ==  38)
sol.add(b + f + k + p ==  38)
sol.add(a + e + j + o + s == 38)  
sol.add(d + i + n + r ==  38)
sol.add(h + m + q ==  38)

sol.add(a < c)
sol.add(a < h)
sol.add(a < l)
sol.add(a < q)
sol.add(a < s)
sol.add(c < h)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print([mod.eval(LD[i]) for i in range(N)])
  getDifferentSolution(sol,mod,LD)

print("num_solutions:", num_solutions)
