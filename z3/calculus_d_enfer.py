#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Calculs d'enfer puzzle in Z3
# Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
# http://citeseer.ist.psu.edu/161721.html
# 
# The solution is the manual is:
# """
# a = -16, b = -14, c = -13, d = -12, e = -10,
# f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
# l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
# r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
# x = 7, y = -2, z = -5.
#
# max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
# """
#
# Also, see the discussion of the Z model:
# http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
# (which shows the same solution).

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

N = 26
min_val = -100
max_val = 100

sol = Optimize() # much faster: 1.3s
# sol = Solver() # slower: 3.6s

a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z = Ints("a b c d e f g h i j k l m n o p q r s t u v w x y z")
A = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]

for I in range(N):
    sol.add(A[I] >= min_val, A[I] <= max_val)

a_max = Int("a_max")
sol.add(a_max >= 0, a_max <= N)

a_abs = [Int("a_abs_%i"%I)  for I in range(N)]
for I in range(N):
    sol.add(a_abs[I] >= min_val, a_abs[I] <= max_val)

for I in range(N):
    sol.add(a_abs[I] == Abs(A[I]))

maximum(sol, a_max, a_abs)

sol.add(Distinct(A))

sol.add(z+e+r+o == 0)
sol.add(o+n+e == 1)
sol.add(t+w+o == 2)
sol.add(t+h+r+e+e == 3)
sol.add(f+o+u+r == 4)
sol.add(f+i+v+e == 5)
sol.add(s+i+x == 6)
sol.add(s+e+v+e+n == 7)
sol.add(e+i+g+h+t == 8)
sol.add(n+i+n+e == 9)
sol.add(t+e+n == 10)
sol.add(e+l+e+v+e+n == 11)
sol.add(t+w+e+l+f == 12)

sol.minimize(a_max)

if  sol.check() == sat:
    mod = sol.model()
    print "a_max:", mod.eval(a_max)
    print "A:", [mod.eval(A[I]) for I in range(N)]
    # getLessSolution(sol,mod,a_max)

    

