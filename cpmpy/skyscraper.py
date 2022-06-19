"""
Skyscraper puzzle in cpmpy.

See 
http://logicgames.blogspot.com/2008/06/skyscraper.html
http://www.puzzlepicnic.com/genre?flats
http://www.puzzlepicnic.com/puzzle?909


Example:

 Puzzle:
   1 2 2 3
 1         4
 3         2
 2         2
 3         1
   3 2 2 1
 
 Solution:
   1 2 2 3
 1 4 3 2 1 4 
 3 1 2 4 3 2 
 2 3 4 1 2 2 
 3 2 1 3 4 1
   3 2 2 1


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



#
# number of seen skyscrapes
#
def num_skyscrapes(y,num):
  n = len(y)  
  constraints = []
  if num > 0:    
    # Count all the positions where y[i] is larger then all y[0..i]
    constraints += [num == 1 + sum([i == sum([y[i] > y[j] for j in range(i)]) for i in range(1,n)])]

  return constraints


def skyscraper(problem):

  cu = problem["cu"]
  cl = problem["cl"]
  rl = problem["rl"]
  rr = problem["rr"]
  
  n = len(cu)

  # variables
  x = intvar(1,n,shape=(n,n),name="x")


  model = Model(latin_square(x))

  # check the row and column constraints
  for i in range(n):
    rl_a = intvar(1,n,shape=n)
    rr_a = intvar(1,n,shape=n)
    cu_a = intvar(1,n,shape=n)
    cl_a = intvar(1,n,shape=n)
  
    # rows
    for j in range(n):
      model += [rl_a[j] == x[i,j],
                num_skyscrapes(rl_a, rl[i]),
                reverse(rl_a, rr_a),
                num_skyscrapes(rr_a, rr[i])
                ]

    # columns
    for j in range(n):
      model += [cu_a[j] == x[j,i],
                num_skyscrapes(cu_a, cu[i]),
                reverse(cu_a, cl_a),
                num_skyscrapes(cl_a, cl[i])
                ]


  # print(model)

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=x)
  print("num_solutions:", num_solutions)
  print()


skyscraper_problems = {
  
  # The problem above
  #   Puzzle:
  #   1 2 2 3
  # 1         4
  # 3         2
  # 2         2
  # 3         1
  #   3 2 2 1
  #
  # Solution:
  #   1 2 2 3
  # 1 4 3 2 1 4 
  # 3 1 2 4 3 2 
  # 2 3 4 1 2 2 
  # 3 2 1 3 4 1
  #   3 2 2 1

  "problem1": {
  "cu" : [1,2,2,3], # column upper
  "cl" : [3,2,2,1], # column lower
  "rl" : [1,3,2,3], # row left
  "rr" : [4,2,2,1], # row right

  },
  
  "problem2" : {
  # From 
  # http://www.puzzlemix.com/playgrid.php?id=74166&type=sky&share=1
  # (Skyscraper-5 87)
  "cu" : [4,0,0,0,4], # column upper
  "cl" : [0,2,3,0,0], # column lower
  "rl" : [0,0,0,0,0], # row left
  "rr" : [3,0,0,0,2], # row right

  },

  #
  # Problem from http://logicgames.blogspot.com/2008/06/skyscraper.html
  #        1 2
  #       x x x x
  #    3  x x x x
  #    3  x x x x 2
  #       x x x x
  #
  #    Solution:
  #    3 4 2 1
  #    2 3 1 4
  #    1 2 4 3
  #    4 1 3 2
  #
  "problem3" : {
  "cu" : [0,1,2,0], # column upper
  "cl" : [0,0,0,0], # column lower
  "rl" : [0,3,3,0], # row left
  "rr" : [0,0,2,0], # row right

  },

  # From http://www.puzzlemix.com/Skyscraper
  "problem4": {
  "cu" : [4,2,1,2,3],
  "cl" : [1,4,3,2,2],
  "rl" : [3,2,3,2,1],
  "rr" : [3,4,1,2,2],

  }
}

for p in skyscraper_problems:
  print(f"problem {p}")
  skyscraper(skyscraper_problems[p])
