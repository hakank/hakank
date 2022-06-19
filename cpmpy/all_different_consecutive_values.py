"""
Global constraint all_different_consecutive_values in cpmpy.

From Global constraint catalogue:
http://www.emn.fr/z-info/sdemasse/gccat/Calldifferent_consecutive_values.html
'''
Purpose:
  Enforce 
   (1) all variables of the collection VARIABLES to take distinct values and 
   (2) constraint the difference between the largest and the smallest values 
       of the VARIABLES collection to be equal to the number of variables 
       minus one (i.e., there is no holes at all within the used values).

Example: (5,4,3,6)
''' 



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""

import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def all_different_consecutive_values_test(n=4,m=10):
    
    x = intvar(1,m,shape=n,name="x")
  
    model = Model(all_different_consecutive_values(x))
  
    num_solutions = model.solveAll(display=x)
    print("num_solutions:", num_solutions)


all_different_consecutive_values_test()
