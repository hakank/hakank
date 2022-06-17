"""
Initials queue puzzle in cpmpy.

From Chris Smith' Math Newsletter #568
'''
I noticed something weird about names of the first 10 people in
the queue for a Maths lecture:
[Image of 10 people in a queue.]

Each of them had initials which were an alphabetically ordered
pair of distinct letters from the first five letters of the
alphabet (A,B,C,D,E)! None of the people had the same initials as
any other and no-one shared a letter with the person in front of
them. Mathematician BE was at the front of the queue with CD
right behind, while BD was right at the end of the queue.
Can you use this information to work out the initials of the ten
people in the queue?
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def initial_pair_queue():
  
  n = 10
  A = 0; B = 1; C = 2; D = 3; E = 4

  queue = intvar(A,E,shape=(n,2),name="queue")

  # constraints
  model = Model([
    # alphabetically ordered pairs of distinct letters
    [queue[i,0] < queue[i,1] for i in range(n)],
    
    # None of the people had the same initials as any other
    all_different_pairs(queue,n),
    
    # no-one shared a letter with the person in front of them    
    [ AllDifferent([queue[i,0],queue[i,1],queue[i+1,0],queue[i+1,1]]) for i in range(n-1)],
    
    queue[0,0] == B, # BE in front
    queue[0,1] == E,
    
    queue[1,0] == C, # CD right behind
    queue[1,1] == D,
    
    queue[n-1,0] == B, # BD at the end
    queue[n-1,1] == D,
    ]
    )
  
  def print_sol():
    q = queue.value()
    print("queue:\n")
    print(q)
    print("queue:\n", [ cs[q[i,0]]+cs[q[i,1]] for i in range(n)])
    print()

  cs = "ABCDE"
  ss = SolverLookup.get('ortools', model)  
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)
  print(ss.status())

initial_pair_queue()
