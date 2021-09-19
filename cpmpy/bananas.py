"""
Bananas problem in cpmpy.

'''
In three dollars, you get 5 bananas, in five dollars, 7 oranges, in
seven dollars, 9 mangoes and in nine dollars, three apples, I need to
purchase 100 fruits in 100 dollars. Please keep in mind that all type
of fruits need to be purchased but I do not like banana and apple, so
these should be of minimum quantity.
'''

(Note: I have forgot where I got this problem.)


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort


def bananas():

  x = intvar(1,100,shape=4,name="x")
  bananas,oranges,mangoes,apples = x
  
  the_sum = intvar(1,2000,name="the_sum")

  model = Model([the_sum == bananas+apples,
                 # This don't work since "/" does integer division
                 # 3*bananas/5 + 5*oranges/7 + 7*mangoes/9 + 9*apples/3 == 100,
                 # we multiply with 3*5*7*9=945 on both sides to weed out the divisions
                 3*bananas*189 + 5*oranges*135 + 7*mangoes*105 + 9*apples*315 == 100*945,
                 sum(x) == 100
                 ])

  model.minimize(the_sum)
  ss = CPM_ortools(model)
  if ss.solve():
    print("the_sum:", the_sum.value())
    print("bananas:", bananas.value())
    print("oranges:", oranges.value())
    print("mangoes:", mangoes.value())
    print("apples :", apples.value())
    print()


bananas()
