"""
No Time to Try puzzle in cpmpy.

https://twitter.com/robeastaway/status/1447627451874545665
'''
A cracking James Blond puzzle by @stecks in this week’s New Scientist

  Puzzle 
  set by Katie Steckles
  #134 No time to try 

  James Blond edges along the corridors of the supervillan's base, and 
  comes to two locked doors, each with a keypad that requires a four-digit
  code. He will need to get through one of the doors, but there is no time
  to guess a four-digit code - the number of possible combinations is 
  staggering. 

  But wait! Some of the buttons on the keypads are visible worn down,
  while others look as if they have never been pressed.

    [ 
       Left: 1,5,6,0 is worn down     Right: 2,6,7 is worn down. 
    ]

  One door has a keypad with four worn buttons, the other has three.
  Blond only has time to try one door, and he will have to try all
  the possible combinations.

  Which of the two keypads will give him fewer combinations to try - 
  the one with four worn buttons, or the one with three?

'''

Here are two different approaches, one plain Python program using permutations,
and one using CP. Both give the same solution.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import itertools
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

#
# Using CP
#
def no_time_to_try_cp(buttons):

  print("buttons:",buttons)

  n = 4
  x = intvar(0,9,shape=n,name="x")

  model = Model()
  for i in range(len(buttons)):
      model += [member_of(x,buttons[i])]

  if len(buttons) == 4:
      model += [AllDifferent(x)]
  else:
      model += [nvalue(3,x)]

  def print_sol():
    print(x.value(),end=" ")    

  ss = CPM_ortools(model) 

  num_solutions = ss.solveAll(display=print_sol)
  print()
  print("num_solutions:", num_solutions)  

#
# Using permutations
#
def no_time_to_try_permutations(buttons):
    print("buttons:", buttons)
    if len(buttons) == 4:
        perms = list(itertools.permutations(buttons))
        print(perms)
        print("len:",len(perms))
    else:
        perms = []
        for b in buttons:
            perms += list(itertools.permutations([b] + buttons))
        perms = list(set(perms))
        print("perms:",perms)
        print("len:",len(perms))
  
    
worn_buttons = [[1,5,6,0],
                [2,6,7]
                ]
print("Using CP")
for buttons in worn_buttons:
  no_time_to_try_cp(buttons)
  print()

print("Using permutations")
for buttons in worn_buttons:
    no_time_to_try_permutations(buttons)

