"""
Curious numbers in cpmpy.
'''
Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.

The number 48 has this peculiarity, that if you add 1 to it the result
is a square number, and if you add 1 to its half, you also get a
square number. Now, there is no limit to the numbers that have this
peculiarity, and it is an interesting puzzle to find three more of
them---the smallest possible numbers. What are they?
'''


The least such numbers are: 
[
 [48,49,7,24,25,5],
 [1680,1681,41,840,841,29],
 [57120,57121,239,28560,28561,169], 
 [1940448,1940449,1393,970224,970225,985]
]

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def cur_num():

  n = 6
  s = intvar(1,2_000_000,shape=n,name="s")
  x,a,b,c,d,e = s
  
  model = Model([
      x + 1 == a, # if you add 1 to it 
      a == b * b, # the result is a square number
      
      x == 2 * c, # if you to its half
      c + 1 == d, # add 1 
      d == e * e, # you also get a square number
 
      ])

  cur_nums = []
  def print_sol():
    print("s:", s.value())
    x_val = x.value()
    if x_val != 48:
        cur_nums.append(x_val)
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)
  print("The other cur_nums (besides 48):", cur_nums)


cur_num()
