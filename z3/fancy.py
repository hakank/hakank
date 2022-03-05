#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Mr Greenguest puzzle (a.k.a fancy dress problem) in Z3
#
# Problem (and LPL) code in
#
# http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
#
# """
# (** Mr. Greenfan wants to give a dress party where the male guests
#  * must wear green dresses. The following rules are given:
#  * 1 If someone wears a green tie he has to wear a green shirt.
#  * 2 A guest may only wear green socks and a green shirt 
#  *   if he wears a green tie or a green hat.
#  * 3 A guest wearing a green shirt or a green hat or who does
#  *   not wear green socks must wear a green tie.
#  * 4 A guest who is not dressed according to rules 1-3 must
#  *   pay a $11 entrance fee.
#  * Mr Greenguest wants to participate but owns only a green shirt 
#  * (otherwise he would have to pay one for $9). He could buy 
#  * a green tie for $10, a green hat (used) for $2 and green socks
#  * for $12.
#  * What is the cheapest solution for Mr Greenguest to participate?
#  *)
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("QF_FD")

# variables
# t: tie
# h: hat
# r: shirt
# s: socks
# n: entrance fee
[t,h,r,s,n] = Bools('t h r s n')
cost = makeIntVar(sol,"cost",0,100)

# constraints


# This is a straight translation from the LPL code
# ( (t->r) \/ n) 
sol.add( Or(Implies(t,r),  n))
# ( ((s \/ r) -> (t \/ h)) \/ n )
sol.add( Or(Implies(Or(s,r), Or(t,h)), n))
# ( ((r \/ h \/ not s) -> t) \/ n )
sol.add(Or( Implies(Or(r, h, Not(s)), t), n))

sol.add(cost == 10*t + 2*h + 12*s + 11*n)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("cost:", mod.eval(cost),end=" ")
  print([(x, mod.eval(x)) for x in [t,h,r,s,n]])
  getLessSolution(sol,mod,cost)
