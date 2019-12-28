#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# global constraint circuit (decomposition) in Z3
# It also generates the path in the circuit (the z array)
#
# Note that it requires arrays of type Arrays since we use the element approach
#   ( z = x[y], where y is a decision variable)
#
# See the definition in http://hakank.org/z3/z3_utils_hakank.py
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import * 

def test_circuit(n,print_all=True):
    sol = Solver()

    x = makeIntArray(sol, 'x',n, 1, n)
    z = makeIntArray(sol, 'z',n, 1, n)

    # circuit is defined in z3_utils_hakank
    circuit(sol,x,z, n)

    # Show all solutions
    count = 0
    while sol.check() == sat:
        m = sol.model()
        count += 1
        xx = [m.eval(x[i]) for i in range(n)]
        sol.add(Or([x[i] != xx[i] for i in range(n)]))
        if print_all:
            zz = [m.eval(z[i]) for i in range(n)]
            print("x: ", xx)
            print("z: ", zz)
            print()
    
    print("n=",n, " count:", count)


test_circuit(4,True)    
for n in range(1,8):
    test_circuit(n,False)
