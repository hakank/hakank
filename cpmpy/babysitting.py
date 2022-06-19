"""
Babysitting puzzle (Dell Logic Puzzles) in cpmpy.

Problem from http://brownbuffalo.sourceforge.net/BabysittingClues.html
'''
Title: Babysitting
Author: Scott Marley
Publication: Dell Logic Puzzles
Issue: April, 1998
Page: 7
Stars: 1

Each weekday, Bonnie takes care of five of the neighbors' children. 
The children's names are Keith, Libby, Margo, Nora, and Otto; last names 
are Fell, Gant, Hall, Ivey, and Jule. Each is a different number of years 
old, from two to six. Can you find each child's full name and age?
#
1. One child is named Libby Jule.
2. Keith is one year older than the Ivey child, who is one year older 
   than Nora.
3. The Fell child is three years older than Margo.
4. Otto is twice as many years old as the Hall child.

Determine: First name - Last name - Age 
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def babysitting():

  n = 5

  first = range(n)
  Keith, Libby, Margo, Nora, Otto = first

  # variables
  last = intvar(0,n-1, shape=n,name="last")
  Fell, Gant, Hall, Ivey, Jule = last
  
  age = intvar(2,6,shape=n,name="age")

  # constraints
  model = Model([AllDifferent(last),
                 AllDifferent(age),

                 #  1. One child is named Libby Jule.
                 Jule == Libby,

                 #  2. Keith is one year older than the Ivey child, who is one 
                 #     year older than Nora.
                 age[Keith] == age[Ivey] + 1,
                 Keith != Ivey,

                 age[Ivey] == age[Nora] + 1,
                 Ivey != Nora,

                 #  3. The Fell child is three years older than Margo.
                 age[Fell] == age[Margo] + 3,
                 Fell != Margo,

                 #  4. Otto is twice as many years old as the Hall child.
                 age[Otto] == age[Hall]*2,
                 Otto != Hall,
                 ])

  def print_sol():
    print("last:",last.value())
    print("age:",age.value())  

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

babysitting()


