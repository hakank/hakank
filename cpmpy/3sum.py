"""
3SUM (Three Elements That Sum To Zero) in cpmpy.

From
http://nathanleclaire.com/blog/2013/10/22/three-elements-that-sum-to-zero/
'''
Given a collection of integers, return the indices of any three elements which sum to zero. 
For instance, if you are given {-1, 6, 8, 9, 10, -100, 78, 0, 1}, you could return {0, 7, 8} 
because -1 + 1 + 0 == 0. You can't use the same index twice, and if there is no match you 
should return {-1, -1, -1}.
'''

Also see: https://en.wikipedia.org/wiki/3SUM


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def three_sum(nums):
  print("\nnums:",nums)
  model = Model()
  
  n = len(nums)

  m = 3 # the number of elements that should sum to 0

  x = boolvar(shape=n,name="x")
    
  model += (sum([nums[i]*x[i] for i in range(n)]) == 0)
  model += (sum(x) == m)

  def print_sol():
    xs = x.value()
    # print("x:",xs)
    print("indices:",[i for i in range(n) if xs[i]])
    print("nums:",[nums[i] for i in range(n) if xs[i]])
    print()


  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

sat_nums = [-1, 6, 8, 9, 10, -100, 78, 0, 1] # SAT
unsat_nums = [1, 6, 8, 9, 10, 100, 78, 0, 1] # UNSAT
random_nums = [random.randint(-10,10) for _ in range(20)]

three_sum(sat_nums)
three_sum(unsat_nums)
three_sum(random_nums)
