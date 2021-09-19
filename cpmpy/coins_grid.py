"""
Coins grid problem in cpmpy.
  
Problem from 
Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
'''
In a quadratic grid (or a larger chessboard) with 31x31 cells, one should
 place coins in such a way that the following conditions are fulfilled:
   1. In each row exactly 14 coins must be placed.
   2. In each column exactly 14 coins must be placed.
   3. The sum of the quadratic horizontal distance from the main 
      diagonal of all cells containing a coin must be as 
      small as possible.
   4. In each cell at most one coin can be placed.

The description says to place 14x31 = 434 coins on the chessboard 
each row containing 14
coins and each column also containing 14 coins.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *


def coins_grid(n, c):

    x = intvar(0,1,shape=(n, n), name="x")
    z = intvar(0, 1000000,name="z")

    model = Model([
        # every row adds up to c
        [sum(row) == c for row in x],
        
        # every col adds up to c
        [sum(col) == c for col in x.transpose()],
        
        # quadratic horizonal distance
        z == sum([x[i,j]*abs(i-j)*abs(i-j) for i in range(0,n) for j in range(0,n)]),
        
        ],minimize=z
        )

    ss = CPM_ortools(model)
    if ss.solve():
        print("z: ", z.value())
        print("x:\n",x.value())
        print()

n = 31
c = 14
coins_grid(n, c)
