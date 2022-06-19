"""
Enigma 1224 - Age-changing in cpmpy.

https://enigmaticcode.wordpress.com/2015/06/20/enigma-1224-age-changing/
'''
From New Scientist #2380, 1st February 2003

    If you start with my age, in years, and apply the four operations:

    [  
       +2  /8 

       -3  *7

    ]

    in some order, then the final answer you get is my husband’s age in years.

    Funnily enough, if you start with his age and apply the same four operations in a 
    different order, then you get my age.

    What are our two ages?
'''

There are two solutions:

    m: 53 h: 48
    hlist: [53, 371, 368, 46, 48]
    mlist: [48, 6, 8, 56, 53]
    perm1: ['*7', '-3', '/8', '+2']
    perm2: ['/8', '+2', '*7', '-3']

    m: 48 h: 53
    hlist: [48, 6, 8, 56, 53]
    mlist: [53, 371, 368, 46, 48]
    perm1: ['/8', '+2', '*7', '-3']
    perm2: ['*7', '-3', '/8', '+2']

However, if we add the constraint that m < h then there's only one
solution, the latter.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""

import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def check(perm, old, new):
    return [
        (perm == 0).implies(new == old + 2),
        # (perm == 1).implies(new == old / 8), # This give a lot of bad solutions
        (perm == 1).implies(8*new == old),     # This works 
        (perm == 2).implies(new == old - 3),
        (perm == 3).implies(new == old * 7)
        ]

def age_changing():

  n = 4

  perms = ["+2","/8","-3","*7"]

  # ages 16..120
  age_low = 16
  age_high = 120

  # variables
  m = intvar(age_low, age_high,name="m") # my age
  h = intvar(age_low, age_high,name="h") # my age

  perm1 = intvar(0,n-1,shape=n,name="perm1")
  perm2 = intvar(0,n-1,shape=n,name="perm2")

  # for calculating my age and husband's age
  mlist = intvar(1,1000,shape=n+1,name="mlist")
  hlist = intvar(1,1000,shape=n+1,name="hlist")

  # constraints
  model = Model([AllDifferent(perm1),
                 AllDifferent(perm2),

                 # same operations in different order
                 sum([perm1[i] != perm2[i] for i in range(n)]) > 0,

                 # find husbands age, start with my age
                 hlist[0] == m,

                 # husband's age is last in hlist
                 h == hlist[n],

                 # checking my age, start with husband's age
                 mlist[0] == h,

                 # my age is last in mlist
                 m == mlist[n],

                 # check the operations
                 [check(perm1[i], hlist[i], hlist[i+1]) for i in range(n)],
                 [check(perm2[i], mlist[i], mlist[i+1]) for i in range(n)],

                 # Symmetry breaking: I'm younger than husband
                 # m < h
                 ])

  def print_sol():
      print("m:", m.value(), "h:",h.value())        
      print("hlist:", hlist.value())
      print("mlist:", mlist.value())
      print("perm1:", [perms[perm1[i].value()] for i in range(n)])
      print("perm2:", [perms[perm2[i].value()] for i in range(n)])
      print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  


age_changing()
