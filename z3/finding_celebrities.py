#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Finding celebrities problem in Z3
#
# From Uwe Hoffmann
# "Finding celebrities at a party"
# http://www.codemanic.com/papers/celebs/celebs.pdf
# """
# Problem: Given a list of people at a party and for each person the list of
# people they know at the party, we want to find the celebrities at the party. 
# A celebrity is a person that everybody at the party knows but that 
# only knows other celebrities. At least one celebrity is present at the party.
# """
# (This paper also has an implementation in Scala.)
#
# Note: The original of this problem is 
#   Richard Bird and Sharon Curtis: 
#   "Functional pearls: Finding celebrities: A lesson in functional programming"
#   J. Funct. Program., 16(1):13 20, 2006.
#
# The problem from Hoffmann's paper is to find of who are the 
# celebrity/celebrities in this party graph:
#   Adam  knows {Dan,Alice,Peter,Eva},
#   Dan   knows {Adam,Alice,Peter},
#   Eva   knows {Alice,Peter},
#   Alice knows {Peter},
#   Peter knows {Alice}
#
# Solution: the celebrities are Peter and Alice.
#
# I blogged about this problem in "Finding celebrities at a party"
# http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html


# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def finding_celebrities(problem):

  graph = problem
  n = len(graph)
  
  sol = Solver()

  # variables
  
  celebrities = makeIntVector(sol,"celebrities",n,0,1) # 1 if a celebrity
  num_celebrities = makeIntVar(sol,"num_celebrities",0,n)

  # constraints

  sol.add(num_celebrities == Sum(celebrities))
   
  # All persons know the celebrities,
  # and the celebrities only know celebrities.
  for i in range(n):
     sol.add((celebrities[i] == 1) == (Sum([If(graph[j][i] == 1,1,0) for j in range(n)]) == n))
     sol.add((celebrities[i] == 1) == (Sum([If(graph[i][j] == 1,1,0) for j in range(n)]) == num_celebrities))


  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("num_celebrities :", mod.eval(num_celebrities))
    print("celebrities  :", [i for i in range(n) if mod.eval(celebrities[i]) == 1])
    print()
    getDifferentSolution(sol,mod,celebrities)

  print("num_solutions:", num_solutions)
  print()



#
# The party graph of the example above:
#
#  Adam  knows [Dan,Alice,Peter,Eva],  [2,3,4,5]
#  Dan   knows [Adam,Alice,Peter],     [1,4,5]
#  Eva   knows [Alice,Peter],     [4,5]
#  Alice knows [Peter],      [5]
#  Peter knows [Alice]       [4]
#
# Solution: Peter and Alice (4,5) are the celebrities.
#
problem1 = [[1,1,1,1,1], # 1
            [1,1,0,1,1], # 2
            [0,0,1,1,1], # 3
            [0,0,0,1,1], # 4
            [0,0,0,1,1]  # 5
            ]



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
