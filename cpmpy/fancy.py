"""
Mr Greenguest puzzle (a.k.a fancy dress problem) in cpmpy.

Problem (and LPL) code in

http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
'''
Mr. Greenfan wants to give a dress party where the male guests
must wear green dresses. The following rules are given:
  1 If someone wears a green tie he has to wear a green shirt.
  2 A guest may only wear green socks and a green shirt 
    if he wears a green tie or a green hat.
  3 A guest wearing a green shirt or a green hat or who does
    not wear green socks must wear a green tie.
  4 A guest who is not dressed according to rules 1-3 must
    pay a $11 entrance fee.
Mr Greenguest wants to participate but owns only a green shirt 
(otherwise he would have to pay one for $9). He could buy 
a green tie for $10, a green hat (used) for $2 and green socks
for $12.
What is the cheapest solution for Mr Greenguest to participate?
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def fancy():


  # variables
  # t: tie
  # h: hat
  # r: shirt
  # s: socks
  # n: entrance fee
  t = boolvar(name="t")
  h = boolvar(name="h")
  r = boolvar(name="r")
  s = boolvar(name="s")
  n = boolvar(name="n")
  cost = intvar(0,100,name="cost")

  model = Model(minimize=cost)
  
  # constraints
  # This is a straight translation from the LPL code
  # ( (t->r) \/ n) 
  model += [ t.implies(r) |  n]
  # ( ((s \/ r) -> (t \/ h)) \/ n )
  model += [ ( (s | r).implies(t|h)) | n]
  # ( ((r \/ h \/ not s) -> t) \/ n )
  model += [(r | h | ~(s)).implies(t | n)]
  
  model += [cost == 10*t + 2*h + 12*s + 11*n]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("cost:",cost.value())
    print("t:",t.value(),"h:",h.value(),"r:",r.value(),"s:",s.value(),"n:",n.value())

  print("num_solutions:", num_solutions)


fancy()
