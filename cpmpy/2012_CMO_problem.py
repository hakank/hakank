"""
  Number theory problem in 2012 CMO in cpmpy.

  http://community.wolfram.com/groups/-/m/t/793922
  '''
  Given two positive integers a and b. The two numbers satisfy the following conditions: 
  a-b is a prime number p and a×b is a perfect square n^2 . 
  Find the smallest value of a no less than 2012.
  '''
    
  
This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
from cpmpy.solvers.utils import get_supported_solvers
import numpy as np
from cpmpy_hakank import *


def m2012_CMO_problem():

  max_val = 10000
  prime_list = primes(2012)

  a = intvar(2012,max_val,name="a")
  b = intvar(2,2012,name="b")
  n = intvar(2,2012,name="n")
  # p can only take primes (reduced below).
  # How do one create a list of specific domain in cpmpy?  
  p = intvar(2,2012,name="p")

  model = Model(minimize=a)

  for i in range(max(prime_list)):
    # if not i in prime_list:
    #  model += [p != i]
    if not is_prime(i):
      model += [p != i]

  model += [a >= b]
  model += [p == a-b]
  model += [a*b == n*n]  

  ss = CPM_ortools(model)
  if ss.solve():
    print("a:",a.value(), "(the answer of the question)")
    print("b:",b.value())
    print("n:",n.value())
    print("p:",p.value())


m2012_CMO_problem()
