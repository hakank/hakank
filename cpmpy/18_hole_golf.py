"""
Can you Golf Golf? in cpmpy.

http://codegolf.stackexchange.com/questions/8429/can-you-golf-golf/
'''
You are required to generate a random 18-hole golf course.

Example output:

[3 4 3 5 5 4 4 4 5 3 3 4 4 3 4 5 5 4]

Rules:

  - Your program must output a list of hole lengths for exactly 18 holes
  - Each hole must have a length of 3, 4 or 5
  - The hole lengths must add up to 72 for the entire course
  - Your program must be able to produce every possible hole configuration with 
    some non-zero-probability (the probabilities of each configuration need not be 
    equal, but feel free to claim extra kudos if this is the case)
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

# a little more golfed: 78 chars
# Though this is not a complete program since it don't import stuff and
# use the nice wrapper for showing all solutions etc...
def g():x=intvar(3,5,shape=18);m=Model(sum(x)==72);ortools_wrapper2(model,[x])


def eighteen_hole_golf(num_sols=0):
    x = intvar(3,5,shape=18,name="x")
    model = Model(sum(x) == 72)  
    num_solutions = model.solveAll(solution_limit=num_sols, display=x)
    print("num_solutions:", num_solutions)

# eighteen_hole_golf(0) # Generate all solutions
eighteen_hole_golf(123)


