"""
Four number problem in cpmpy.

From
http://stackoverflow.com/questions/17720465/given-4-numbers-of-array-of-1-to-10-elements-find-3-numbers-whose-sum-can-gener
'Given 4 numbers of array of 1 to 10 elements. 
 Find 3 numbers whose sum can generate all the four numbers?'
'''
I am given an array containing a list of 4 random numbers (1 to 10 inclusive). 
I am supposed to generate a list of 3 numbers (1 to 10 inclusive) so that I can 
generate all the 4 numbers of the initial list by adding the 3 numbers of 
the generated list.
Someone Please provide an algorithm for doing this. 
'''

For the problem instance mentioned in a comment [1,3,7,8], there are 5 solutions:

  r: [1, 3, 7, 8]
  x: [1, 3, 4]
  ----------
  r: [1, 3, 7, 8]
  x: [1, 2, 5]
  ----------
  r: [1, 3, 7, 8]
  x: [1, 2, 6]
  ----------
  r: [1, 3, 7, 8]
  x: [1, 2, 7]
  ----------
  r: [1, 3, 7, 8]
  x: [1, 3, 7]


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations


def rand_nums(max_n,max_val):
    """
    returns a list of atmost n values in 1..max_val
    """
    return np.unique(np.random.randint(1,max_val,max_n))


def four_numbers(nums,n,max_val=10,num_sols=0):
    print("nums:",nums)
    print("n:",n)
    m = len(nums)

    x = intvar(1,max_val,shape=n,name="x")
    # coefficient matrix
    tmp = boolvar(shape=(m,n),name="tmp")

    model = Model()
    for i in range(m):
        model += [sum([tmp[i,j]*x[j] for j in range(n)]) == nums[i]]

    model += [increasing(x)]
    
    ss = CPM_ortools(model)

    num_solutions = ss.solveAll(solution_limit=num_sols,display=x)
    print("number of solutions:", num_solutions)
    print()

num_sols = 0
nums = [1,3,7,8]
n = 3
four_numbers(nums,n,10,num_sols)
print("\nSome random cases:")
num_sols = 10
for i in range(10):
    nums = rand_nums(7,10)
    n = min([3,len(nums)-1])
    four_numbers(nums,n,10,num_sols)
