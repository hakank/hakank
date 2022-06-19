"""
Mama' age problem in cpmpy.

Mamma's Age from 'Amusements in Mathematics, Dudeney', number 40.
'''
Tommy: 'How old are you, mamma?'
Mamma: 'Our three ages add up to exactly seventy years.'
Tommy: 'And how old are you, papa?'
Papa: 'Just six times as old as you, my son.'
Tommy: 'Shall I ever be half as old as you, papa?'
Papa: 'Yes, Tommy; and when that happens our three ages will add up to
       exactly twice as much as today.'

Can you find the age of Mamma?
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def mamas_age():

  m = intvar(1,500,name="m") # mamma's age
  p = intvar(1,500,name="p") # papa's age
  t = intvar(1,500,name="t") # tommy's age
  i = intvar(1,500,name="i") # temp

  # the real ages
  m2 = intvar(1,100,name="m2") # mamma's age
  p2 = intvar(1,100,name="p2") # papa's age
  t2 = intvar(1,100,name="t2") # tommy's age

  model = Model([m + p + t == 70 * 12,
                 6 * t == p,
                 (t + i) * 2 == p + i,
                 m + i + p + i + t + i == 2 * (m + p + t),
                 m2 == m / 12,
                 p2 == p / 12,
                 t2 == t / 12
                 ])

  def print_sol():
    print("mamma's age:", m2.value())
    print("papa's age:", p2.value())
    print("tommy's age:", t2.value())
    
  
  ss = CPM_ortools(model)
  ss.solveAll(display=print_sol)
      

mamas_age()
