"""
Pandigital numbers in cpmpy.

From Albert H. Beiler "Recreations in the Theory of Numbers", quoted from
http://www.worldofnumbers.com/ninedig1.htm
'''
Chapter VIII : Digits - and the magic of 9 

The following curious table shows how to arrange the 9 digits so that 
the product of 2 groups is equal to a number represented by the 
remaining digits.

  12 x 483 = 5796 
  42 x 138 = 5796 
  18 x 297 = 5346 
  27 x 198 = 5346 
  39 x 186 = 7254 
  48 x 159 = 7632 
  28 x 157 = 4396 
  4 x 1738 = 6952 
  4 x 1963 = 7852
'''

See also MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
A number is said to be pandigital if it contains each of the digits 
from 0 to 9 (and whose leading digit must be nonzero). However, 
'zeroless' pandigital quantities contain the digits 1 through 9. 
Sometimes exclusivity is also required so that each digit is 
restricted to appear exactly once.
''

Note: The pandigital numbers problem is thus zeroless pandigital
number and each is restricted to appear exactly once.

* Wikipedia http://en.wikipedia.org/wiki/Pandigital_number


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def pandigital_numbers(base=10, start=1, len1=1, len2=4):

    max_d = base-1
    x_len = max_d+1-start
    max_num = base**4

    # Decision variables
    num1 = intvar(0, max_num,name="num1")
    num2 = intvar(0, max_num,name="num2")
    res  = intvar(0, max_num,name="res")
    x = intvar(start, max_d,shape=x_len,name="x")

    model = Model(
                  AllDifferent(x)
                  )
    
    model += [to_num([x[i] for i in range(len1)], num1, base)]
    model += [to_num([x[i] for i in range(len1,len1+len2)], num2, base)]
    model += [to_num([x[i] for i in range(len1+len2,x_len)], res, base)]
    
    model += [num1*num2 == res]

    # no number must start with 0
    model += [x[0] > 0]
    model += [x[len1] > 0]
    model += [x[len1+len2] > 0]

    # symmetry breaking
    model += [num1 <= num2]

    solutions = []
    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0
    while ss.solve():
        solutions.append(x.value())
        ss += any(x != x.value())
               
    return solutions


def print_solution(x,len1,len2,x_len):
    print("".join([str(x[i]) for i in range(len1)]), "*",end=" ")
    print("".join([str(x[i]) for i in range(len1,len1+len2)]), "=",end=" ")
    print("".join([str(x[i]) for i in range(len1+len2,x_len)]))
    

num_solutions = 0
base = 10
start = 1
x_len = base-1 + 1-start
for len1 in range(1+math.floor(x_len / 3)+1):
  for len2 in range(1+math.floor(x_len / 3)+1):
      if x_len > len1 + len2:
          sol = pandigital_numbers(base, start, len1, len2)
          for s in sol:
              num_solutions += 1
              print_solution(s, len1, len2, x_len)             

print("num_solutions:", num_solutions)
