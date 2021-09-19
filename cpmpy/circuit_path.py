"""
Decomposition of the circuit_path constraint in cpmpy

circuit_path(x,path) is a variant of (my_)circuit were the
path is visible.

The 'orbit' method that is used here is based on some
observations on permutation orbits.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *

def print_solution(a):
    print("x:", a[0].value(), " z:", a[1].value())

def circuit_path_test(n=5):

    x = intvar(0, n-1,n,name='x')
    z = intvar(0, n-1,shape=n,name='z')    
    model = Model (
        my_circuit_path(x,z),
        )

    # O.17s
    ortools_wrapper(model,[x,z],print_solution)
    
    # 0.345s
    # ss = CPM_ortools(model)    
    # num_solutions = 0
    # while ss.solve():
    #     print("x:", x.value(), " z:", z.value())
    #     num_solutions += 1
    #     get_different_solution(ss,flatten_lists([x,z]))
        
    # print(f"num_solutions: {num_solutions}")


circuit_path_test(5)

