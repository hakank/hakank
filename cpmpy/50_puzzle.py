"""
Fifty puzzle (Martin Chlond) in cpmpy.

From Martin Chlond Integer Programming Puzzles:
http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 5. 
Description  : Fifty puzzle
Source       : The Puzzles of Sam Loyd (P 54)
'''
5. A side show at Coney Island is described as follows: "There were ten little 
dummies which you were to knock over with baseballs. The man said: 'Take as many 
throws as you like at a cent apiece and stand as close as you please. Add up the 
numbers on all the men that you knock down and when the sum amounts to exactly 
fifty, neither more nor less you get a genuine 25 cent Maggie Cline cigar with 
a gold band around it.'"
The numbers on the ten dummies were 15, 9, 30, 21, 19, 3, 12, 6, 25, 27. (Loyd)
'''

Answer: 6, 19 and 25


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def main():

  n = 10
  v = [3, 6, 9, 12, 15, 19, 21, 25, 27, 30]

  x = boolvar(shape=n,name="x")
  sumX = intvar(0,n,name="sumX")

  model = Model([sumX == x.sum(),
                 sum([v[i]*x[i] for i in range(n)]) == 50
                 ])

  ss = CPM_ortools(model)
  num_solutions = 0
  while ss.solve():
    num_solutions += 1
    print("x:", [v[i] for i in range(n) if x[i].value() > 0])
    print("sumX:", sumX.value())
    get_different_solution(ss,x)

  print()
  print("num_solutions:", num_solutions)  

main()
