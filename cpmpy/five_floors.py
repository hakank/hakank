"""
Five floors problem in cpmpy.

From Alexey Radul & Gerald Jay Sussman:
'The Art of Propagator', page 34
'''
Baker, Cooper, Fletcher, Miller, and Smith live on the first
five floors of this apartment house. Baker does not live on the
fifth floor. Cooper does not live on the first floor. Fletcher
does not live on either the fifth or the first floor. Miller lives
on a higher floor than does Cooper. Smith does not live on a
floor adjacent to Fletcher'. Fletcher does not live on a floor
adjacent to Cooper's.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations


def five_floors():

    n = 5
    x = intvar(1,n,shape=n,name="x")
    Baker, Cooper, Fletcher, Miller, Smith = x
    xs = ["Baker", "Cooper", "Fletcher","Miller", "Smith"]

    model = Model([AllDifferent(x),

                   # Baker does not live on the fifth floor.
                   Baker != 5,

                   # Cooper does not live on the first floor. 
                   Cooper != 1,

                   # Fletcher does not live on either the fifth or the first floor. 
                   Fletcher != 5,
                   Fletcher != 1,
                   
                   # Miller lives on a higher floor than does Cooper. 
                   Miller > Cooper,
                   
                   # Smith does not live on a floor adjacent to Fletcher'. 
                   abs(Smith-Fletcher) > 1,
                   
                   # Fletcher does not live on a floor adjacent to Cooper's.
                   abs(Fletcher-Cooper) > 1,
                   ])

    def print_sol():
        print("x:",x.value())
        print("floors:",[xs[j] for i in range(n) for j in range(n) if x[j].value()-1 == i ])
        
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("number of solutions:", num_solutions)


five_floors()
