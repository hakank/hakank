"""
Place number puzzle in cpmpy.

http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
""
Place numbers 1 through 8 on nodes
- each number appears exactly once
- no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
""

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/
"""

import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def place_number_puzzle():

    m = 32
    graph =  [
        [1,2],
        [1,3],
        [1,4],
        [2,1],
        [2,3],
        [2,5],
        [2,6],
        [3,2],
        [3,4],
        [3,6],
        [3,7],
        [4,1],
        [4,3],
        [4,6],
        [4,7],
        [5,2],
        [5,3],
        [5,6],
        [5,8],
        [6,2],
        [6,3],
        [6,4],
        [6,5],
        [6,7],
        [6,8],
        [7,3],
        [7,4],
        [7,6],
        [7,8],
        [8,5],
        [8,6],
        [8,7]
        ]
    
    n = 8
    x = intvar(1,n, shape=n,name="x")

    model = Model (
        AllDifferent(x),

        [ abs(x[graph[i][0]-1] - x[graph[i][1]-1]) > 1  for i in range(m)],

        # symmetry breaking
        x[0] < x[n-1]
        )

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=x)    
    print('Number of solutions', num_solutions)



place_number_puzzle()

