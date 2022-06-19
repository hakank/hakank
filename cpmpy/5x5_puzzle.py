"""
5x5 puzzle (Five puzzle, Martin Chlond) in cpmpy.

From http://www.chlond.demon.co.uk/Five.html (Java applet).
(Link from http://www.chlond.demon.co.uk/puzzles/puzzles1.html)
'''
    A B C D E 
    F G H I J 
    K L M N O 
    P Q R S T 
    U V W X Y 

Each of the squares in the above grid can be in one of two states, lit(white)
or unlit(red). If the player clicks on a square then that square and each 
orthogonal neighbour will toggle between the two states. Each mouse click 
constitutes one move and the objective of the puzzle is to light all 
25 squares in the least number of moves.
'''

(Unfortunately, the links above don't work anymore.)

This is a port of Martin Chlond's IP model.

Here are the 4 optimal solutions (via CPM_minizinc):

x:
[[0 1 1 0 1]
 [0 1 1 1 0]
 [0 0 1 1 1]
 [1 1 0 1 1]
 [1 1 0 0 0]]
d:
[[0 1 1 1 0]
 [0 1 2 1 1]
 [0 1 1 2 1]
 [1 1 1 1 1]
 [1 1 0 0 0]]
the_sum: 15

x:
[[0 0 0 1 1]
 [1 1 0 1 1]
 [1 1 1 0 0]
 [0 1 1 1 0]
 [1 0 1 1 0]]
d:
[[0 0 0 1 1]
 [1 1 1 1 1]
 [1 2 1 1 0]
 [1 1 2 1 0]
 [0 1 1 1 0]]
the_sum: 15

x:
[[1 1 0 0 0]
 [1 1 0 1 1]
 [0 0 1 1 1]
 [0 1 1 1 0]
 [0 1 1 0 1]]
d:
[[1 1 0 0 0]
 [1 1 1 1 1]
 [0 1 1 2 1]
 [0 1 2 1 1]
 [0 1 1 1 0]]
the_sum: 15

x:
[[1 0 1 1 0]
 [0 1 1 1 0]
 [1 1 1 0 0]
 [1 1 0 1 1]
 [0 0 0 1 1]]
d:
[[0 1 1 1 0]
 [1 1 2 1 0]
 [1 2 1 1 0]
 [1 1 1 1 1]
 [0 0 0 1 1]]
the_sum: 15




Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
import copy

def five_by_five_puzzle():

  # data
  n = 5

  # decision variabels
  x = boolvar(shape=(n,n),name="x")
  d = intvar(0,n,shape=(n,n),name="d")  
  the_sum = intvar(1,n*n,name="the_sum")
  
  # constraints
  # model = Model(the_sum == sum(x))
  # model = Model(the_sum == [x[i,j] for i in range(n) for j in range(n)])
  model = Model(the_sum == x.sum())

  for i in range(n):
    for j in range(n):
      model += [2*d[i,j]+1 ==
                (sum([x[i,k] for k in range(j-1,j+2) if k in range(n) and k != j])
                + 
                sum([x[k,j] for k in range(i-1,i+2) if k in range(n)])
                 )
              ]

  model_opt = copy.copy(model)
  model_opt.minimize(the_sum)

  model_opt.solve()
  opt = the_sum.value()
  print("Optimal value:",opt)

  model += [the_sum == opt]

  def print_sol():
    print("x:")
    print(1*x.value())
    print("d:")
    print(d.value())
    print("the_sum:",the_sum.value())
    print()

  num_solutions = model.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  


five_by_five_puzzle()
