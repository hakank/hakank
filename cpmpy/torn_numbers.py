"""
Torn numbers in cpmpy.

From
http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/torn.html?19
---
The Torn Number from 'Amusements in Mathematics', Dudeney, number 113

  I had the other day in my possession a label bearing the number 3025
  in large figures. This got accidentally torn in half, so that 30 was
  on one piece and 25 on the other. On looking at these pieces I began
  to make a calculation, scarcely concious of what I was doing, when I
  discovered this little peculiarity. If we add the 30 and the 25
  together and square the sum we get as the result the complete original
  number on the label! Now, the puzzle is to find another number,
  composed of four figures, all different, which may be divided in the
  middle and produce the same result. 
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



def torn_numbers():
    x = intvar(0,9,shape=4,name="x")
    x3, x2, x1, x0 = x
    sumx = intvar(0,9999,name="sumx")

    model = Model([AllDifferent(x),
                   x3 != 0,
                   sumx == x3 * 10 + x2 + x1 * 10 + x0,
                   sumx*sumx == x3 * 1000 + x2 * 100 + x1 * 10 + x0
                   ])

    def print_sol():
        print("x:",x.value(),"sum:",sumx.value())
        
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)    
    print("number of solutions:", num_solutions)


torn_numbers()
