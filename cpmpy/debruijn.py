"""
de Bruijn sequence in cpmpy.

Implementation of de Bruijn sequences in Comet, both "classical" 
and "arbitrary". 

Compare with the the web based programs:
  http://www.hakank.org/comb/debruijn.cgi   
  http://www.hakank.org/comb/debruijn_arb.cgi

Example:
For base = 2, n = 3, m = base^n = 8 there are 2 solutions:
  x       : 0 1 2 5 3 7 6 4 
  bin_code: 0 0 0 1 0 1 1 1 

  x       : 0 1 3 7 6 5 2 4 
  bin_code: 0 0 0 1 1 1 0 1 


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def debruijn(base=2, n=3, m=0):

    if m == 0:
        m = base**n

    print("base:", base, "n:",n, "m:", m)

    #
    # Decision variables
    # Note: Matrix is defined with cols,rows,...
    #
    x        = intvar(0, (base**n)-1,shape=m,name="x")
    binary   = intvar(0, base-1,shape=(m, n),name="binary")
    bin_code = intvar(0, base-1,shape=m, name="bin_code")

    model = Model()

    model += [AllDifferent(x)]

    # convert x[i] <-> binary[i,..]
    model += [[x[i] == sum([binary[i][j]*(base**(n-j-1)) for j in range(n)]) for i in range(m)]]

    # de Bruijn property for the nodes
    model += [[binary[i-1][j] == binary[i][j-1] for j in range(1,n) for i in range(1,m)]]

    # ... and around the corner
    model += [[binary[m-1][j] == binary[0][j-1] for j in range(1,n)]]

    # convert binary -> bin_code
    model += [[bin_code[i] == binary[i][0] for i in range(m)]]

    # symmetry breaking: x[1] is the minimum number
    model += [[x[0] < x[i] for i in range(1,m)]]

    def print_sol():
        print("x: ", x.value())
        print("binary:\n", binary.value())
        print("bin_code:\n", bin_code.value())
        print()
        

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)    
    print()
    print("number of solutions:", num_solutions)

#
# Print the mines
#
def print_matrix(x, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print(x[i][j].value(),end=" ")
        print()



base   = 2
n      = 3
m      = base**n
if len(sys.argv)>1:
    base = int(sys.argv[1])
if len(sys.argv)>2:
    n = int(sys.argv[2])
if len(sys.argv)>3:
    m = int(sys.argv[3])
else:
    m = base**n

debruijn(base, n, m)
