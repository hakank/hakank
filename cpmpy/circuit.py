"""
Decomposition of the circuit constraint in cpmpy

Cf Global constraint catalog:
http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html

Solution of n=4:
x: [2, 0, 3, 1]
x: [3, 0, 1, 2]
x: [1, 3, 0, 2]
x: [3, 2, 0, 1]
x: [1, 2, 3, 0]
x: [2, 3, 1, 0]

The 'orbit' method that is used here is based on some
observations on permutation orbits.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *

def circuit_test(n=5):

    x = intvar(0, n-1,n,name='x')    
    model = Model (
        my_circuit(x),
        )

    num_solutions = model.solveAll(display=x)
    print(f"num_solutions: {num_solutions}")


circuit_test(5)

