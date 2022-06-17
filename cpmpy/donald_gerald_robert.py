"""
DONALD + GERALD = ROBERT problem in cpmpy.

Classic alphametic problem.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def donald_gerald_robert_v1():
    n = 10
    x = intvar(0,9,shape=10,name="x")
    d,o,n,a,l,g,e,r,b,t = x

    model = Model([AllDifferent(x),

                   100000*d + 10000*o + 1000*n + 100*a + 10*l + d 
                   + 100000*g + 10000*e + 1000*r + 100*a + 10*l + d
                   == 100000*r + 10000*o + 1000*b + 100*e + 10*r + t,
                   d > 0,
                   g > 0,
                   r > 0,
                   ])
    
    num_solutions = model.solveAll(display=x)
    print("num_solutions:", num_solutions)


def donald_gerald_robert_v2():
    n = 10
    x = intvar(0,9,shape=10,name="x")
    d,o,n,a,l,g,e,r,b,t = x

    carries = intvar(0,1,shape=6,name="carries")
    c1,c2,c3,c4,c5,c6 = carries

    model = Model([AllDifferent(x),
                   d + d == 10 * c1 + t,
                   c1 + l + l == 10 * c2 + r,
                   c2 + a + a == 10 * c3 + e,
                   c3 + n + r == 10 * c4 + b,
                   c4 + o + e == 10 * c5 + o,
                   c5 + d + g == 10 * c6 + r,

                   d > 0,
                   g > 0,
                   r > 0,
                   c6 == 0, # must be 0 since R is the first digit
                   ])
    
    ss = SolverLookup.get('ortools', model)
    num_solutions=ss.solveAll(display=x)
    print("num_solutions:", num_solutions)
    print(ss.status())


donald_gerald_robert_v1()
donald_gerald_robert_v2()
