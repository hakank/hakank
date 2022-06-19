"""
Jobs puzzle in cpmpy.

(This is a standard problem in Automatic Reasoning.)

From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
'''
Jobs Puzzle

There are four people:  Roberta, Thelma, Steve, and Pete.
 Among them, they hold eight different jobs.
 Each holds exactly two jobs.
 The jobs are chef, guard, nurse, clerk, police officer (gender not implied),
 teacher, actor, and boxer.
 The job of nurse is held by a male.
 The husband of the chef is the clerk.
 Roberta is not a boxer.
 Pete has no education past the ninth grade.
 Roberta, the chef, and the police officer went golfing together.

Question:  Who holds which jobs?
'''

The answer:
Chef       Thelma
Guard      Roberta
Nurse      Steve
Clerk      Pete
Police     Steve
Teacher    Roberta
Actor      Pete
Boxer      Thelma

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def jobs_puzzle():
 
  # data
  n = 4

  persons = range(n)
  [Roberta, Thelma, Steve, Pete] = persons

  person_names = ["Roberta", "Thelma", "Steve", "Pete"] 

  # variables
  Jobs = intvar(0,n-1,shape=2*n,name="Jobs")
  chef, guard, nurse, clerk, police_officer, teacher, actor, boxer = Jobs

  Jobs_s = ["chef", "guard", "nurse", "clerk", "police_officer", "teacher", "actor", "boxer"]

  # constraints

  model = Model( [
    # Each person holds exactly two jobs.
    global_cardinality_count(Jobs,[2,2,2,2]),

    #  The job of nurse is held by a male.
    ((nurse == Steve) | (nurse == Pete)),

    #  The husband of the chef is the clerk.
    ((clerk == Steve) | (clerk == Pete)),
    ((chef == Roberta) | (chef == Thelma)),
    chef != clerk,

    #  Roberta is not a boxer.
    Roberta != boxer,

    #  Pete has no education past the ninth grade.
    Pete != teacher,
    Pete != police_officer,
    Pete != nurse,

    # Roberta, [and] the chef, and the police officer went golfing together.
    Roberta != chef,
    chef != police_officer,
    Roberta != police_officer,

    # From the name of the job
    ((actor == Steve) | (actor == Pete)),
    ])

  def print_sol():
    print("jobs:",Jobs.value())
    print(["%s:%s" % (Jobs_s[i],person_names[Jobs[i].value()]) for i in range(2*n)])
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


jobs_puzzle()
