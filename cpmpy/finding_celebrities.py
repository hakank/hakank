"""
Finding celebrities problem in cpmpy.

From Uwe Hoffmann
'Finding celebrities at a party'
http://www.codemanic.com/papers/celebs/celebs.pdf
'''
Problem: Given a list of people at a party and for each person the list of
people they know at the party, we want to find the celebrities at the party. 
A celebrity is a person that everybody at the party knows but that 
only knows other celebrities. At least one celebrity is present at the party.
'''
(This paper also has an implementation in Scala.)

Note: The original of this problem is 
  Richard Bird and Sharon Curtis: 
  'Functional pearls: Finding celebrities: A lesson in functional programming'
  J. Funct. Program., 16(1):13 20, 2006.

The problem from Hoffmann's paper is to find of who are the 
celebrity/celebrities in this party graph:
  Adam  knows {Dan,Alice,Peter,Eva},
  Dan   knows {Adam,Alice,Peter},
  Eva   knows {Alice,Peter},
  Alice knows {Peter},
  Peter knows {Alice}

Solution: the celebrities are Peter and Alice.

I blogged about this problem in 'Finding celebrities at a party'
http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def random_01_graph(n):
    return [ [random.randint(0,1) for _ in range(n)] for _ in range(n)] 


def finding_celebrities(problem):

  graph = problem
  n = len(graph)
  
  model = Model()

  # variables
  
  celebrities = boolvar(shape=n,name="celebrities") # 1 if a celebrity
  num_celebrities = intvar(0,n,name="num_celebrities")

  # constraints
  model += (num_celebrities == sum(celebrities))
   
  # All persons know the celebrities,
  # and the celebrities only know celebrities.
  for i in range(n):
    # All persons know the celebrities 
    # model += ((celebrities[i] == 1) == (sum([graph[j][i] for j in range(n)]) == n))
    
    # The celebrities only know each other
    # model += ((celebrities[i] == 1) == (sum([graph[i][j] for j in range(n)]) == num_celebrities))

    model += celebrities[i] == ((sum([graph[j][i] for j in range(n)]) == n) & (sum([graph[i][j] for j in range(n)]) == num_celebrities))

  def print_sol():
    print("num_celebrities :", num_celebrities.value())
    print("celebrities  :", [i for i in range(n) if celebrities[i].value() == 1])
    print()
      
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)
  print()



#
# The party graph of the example above:
#
#  Adam  knows [Dan,Alice,Peter,Eva],  [2,3,4,5]
#  Dan   knows [Adam,Alice,Peter],     [1,4,5]
#  Eva   knows [Alice,Peter],          [4,5]
#  Alice knows [Peter],                [5]
#  Peter knows [Alice]                 [4]
#
# Solution: Peter and Alice (4,5) are the celebrities.
#
problem1 = [[1,1,1,1,1], # 1
            [1,1,0,1,1], # 2
            [0,0,1,1,1], # 3
            [0,0,0,1,1], # 4
            [0,0,0,1,1]  # 5
            ]


#
# In this example Alice (4) also knows Adam (1),
# which makes Alice a non celebrity, and since
# Peter (5) knows Alices, Peter is now also a
# non celebrity. Which means that there are no
# celebrities at this party.
# 
problem2 =  [[1,1,1,1,1],
             [1,1,0,1,1],
             [0,0,1,1,1],
             [1,0,0,1,1],
             [0,0,0,1,1]
             ]

#
# Here is another example. It has the following
# cliques:
#  [1,2]
#  [4,5,6]
#  [6,7,8]
#  [3,9,10]
#
# The celebrities are [3,9,10]
#
problem3 = [[0,1,1,0,0,0,0,1,1,1],
            [1,0,1,0,0,0,0,0,1,1],
            [0,0,1,0,0,0,0,0,1,1],
            [0,1,1,0,1,1,0,0,1,1],
            [0,0,1,1,0,1,0,0,1,1],
            [0,0,1,1,1,0,1,1,1,1],
            [0,0,1,0,0,1,0,1,1,1],
            [0,0,1,0,0,1,1,0,1,1],
            [0,0,1,0,0,0,0,0,1,1],
            [0,0,1,0,0,0,0,0,1,1]
            ]

#
# This is the same graph as the one above
# with the following changes:
#   - 9 don't know 3 or 10
# This party graph know consists of just 
# one celebrity: [9]
#
problem4 = [[0,1,1,0,0,0,0,1,1,1],
            [1,0,1,0,0,0,0,0,1,1],
            [0,0,1,0,0,0,0,0,1,1],
            [0,1,1,0,1,1,0,0,1,1],
            [0,0,1,1,0,1,0,0,1,1],
            [0,0,1,1,1,0,1,1,1,1],
            [0,0,1,0,0,1,0,1,1,1],
            [0,0,1,0,0,1,1,0,1,1],
            [0,0,0,0,0,0,0,0,1,0],
            [0,0,1,0,0,0,0,0,1,1]
            ]


print("problem1")
problem = problem1
finding_celebrities(problem)

print("\nproblem2")
problem = problem2
finding_celebrities(problem)

print("\nproblem3")
problem = problem3
finding_celebrities(problem)

print("\nproblem4")
problem = problem4
finding_celebrities(problem)


print("\nrandom problem")
problem = random_01_graph(10)
finding_celebrities(problem)


