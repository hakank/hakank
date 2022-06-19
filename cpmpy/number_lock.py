"""

Number lock problem (Crack the code) in cpmpy.

From Presh Talwalkar (MindYourDecisions) 
'''
Puzzles like this have been shared with the dubious claim that 'only a
genius can solve' them. But they are still fun problems so let's work one
out.

A number lock requires a 3 digit code. Based on these hints, can you crack
the code?

  682 - one number is correct and in the correct position
  645 - one number is correct but in the wrong position
  206 - two numbers are correct but in the wrong positions
  738 - nothing is correct
  780 - one number is correct but in the wrong position

Video:  https://youtu.be/-etLb-8sHBc
'''

Today Moshe Vardi published a related problem
(https://twitter.com/vardi/status/1164204994624741376 )
where all hints, except for the second, where identical:

  682 - one number is correct and in the correct position
  614 - one number is correct but in the wrong position    <-- This is different.
  206 - two numbers are correct but in the wrong positions
  738 - nothing is correct
  780 - one number is correct but in the wrong position

Also, see https://dmcommunity.org/challenge/challenge-sep-2019/


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def check(model, a, b, pos, val):
  """
  `a` and `b`: the two arrays to check (here `b` 
  corresponds to the array `x` in the model).
        
  `pos`: number of correct values and positions
  `val`: numbver correct values 
         (regardless if there are correct position or not)
  """
  n = len(a)

  # number of entries in correct position (and correct values)
  for i in range(n):
    model += (sum([a[j] == b[j] for j in range(n)]) == pos)

  # number of entries which has correct values 
  # (regardless if there are in correct position or not)
  for i in range(n):
    for j in range(n):
      model += (sum([a[j] == b[k] for j in range(n) for k in range(n)]) == val)


def number_lock(problem):

  model = Model()

  # data
  n = len(problem[0][0]) # number of digits
  
  # decision variabels
  x = intvar(0,9,shape=n,name="x")

  # constraints
  for digits, num_correct_pos, num_correct_number in problem:
    check(model, digits, x, num_correct_pos, num_correct_number)

  # search and solution
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:",num_solutions)
  print()
  

# From Presh Talwalkar (MindYourDecisions) 
# See above.
problem1 = [
    [[6,8,2],1,1], # - one number is correct and in the correct position
    [[6,4,5],0,1], # - one number is correct but in the wrong position    
    [[2,0,6],0,2], # - two numbers are correct but in the wrong positions
    [[7,3,8],0,0], # - nothing is correct
    [[7,8,0],0,1]  # - one number is correct but in the wrong position
  ]


# From Moshe Vardi
# See above
problem2 = [
    [[6,8,2],1,1], # - one number is correct and in the correct position
    [[6,1,4],0,1], # - one number is correct but in the wrong position    
    [[2,0,6],0,2], # - two numbers are correct but in the wrong positions
    [[7,3,8],0,0], # - nothing is correct
    [[7,8,0],0,1]  # - one number is correct but in the wrong position
  ]


print("Problem1:")
number_lock(problem1)
  
print("Problem2:")
number_lock(problem2)
