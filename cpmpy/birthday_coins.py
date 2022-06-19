"""
Tommy's Birthday Coins coins puzzle in cpmpy.

From Martin Chlond Integer Programming Puzzles:
http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 2.
Description  : Tommy's Birthday Coins
Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.
'''
2. Tommy was given 15 coins for his birthday, all in half-crowns, shillings 
and sixpences. When he added it up he found that he had 1 5s. 6d. 
How many half-crowns was he given? (Clarke)
'''
Answer: 8 half-crowns, 4 shillings and 3 sixpences
This model was inspired by the XPress Mosel model created by Martin Chlond.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""

import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def birthday_coins():
  
  coin = 3
  value = [30,12,6]
  x = intvar(1,1000,shape=coin,name="x")

  model = Model([
             sum(value*x) == 306,
             sum(x) == 15
             ])

  
  ss = CPM_ortools(model)
  ss.solveAll(display=x)

birthday_coins()

