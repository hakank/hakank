"""
Global constraint sequence in cpmpy.

From the MiniZinc definition:
'''
Requires that in each subsequence 'x[i], ..., x[i + l - 1]' the sum of the
variables is between 'mn' and 'mx'.
    sequence(array[int] of var int: x, int: l, int: mn, int: mx)
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def sequence_test():

    n = 11

    # length of the sequence
    seq_length = 3 
    print("seq_length:",seq_length)

    # decision variables
    x = intvar(1,n,shape=n,name="x")

    # sum of X (just for show)
    xsum = intvar(1,n*n,name="xsum")

  
    lbound = intvar(1,n-1,name="lbound") # lower bound
    ubound = intvar(1,n-1,name="ubound") # upper bound    

    # constraints
    model = Model([xsum == sum(x),
                   sequence(x,seq_length,lbound,ubound),
                   lbound <= ubound,
                   lbound == ubound,
      ])

    def print_sol():
        print("x:", x.value(),"xsum:",xsum.value())
        print("lbound:",lbound.value(),"ubound:",ubound.value(),"seq_length:",seq_length)
        print()

    num_solutions = model.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)

sequence_test()
