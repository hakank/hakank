#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Jobs puzzle in Z3
#
# (This is a standard problem in Automatic Reasoning.)
#
#
# From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
# """
# Jobs Puzzle
# 
# There are four people:  Roberta, Thelma, Steve, and Pete.
#  Among them, they hold eight different jobs.
#  Each holds exactly two jobs.
#  The jobs are chef, guard, nurse, clerk, police officer (gender not implied),
#  teacher, actor, and boxer.
#  The job of nurse is held by a male.
#  The husband of the chef is the clerk.
#  Roberta is not a boxer.
#  Pete has no education past the ninth grade.
#  Roberta, the chef, and the police officer went golfing together.
#
#  Question:  Who holds which jobs?
#
#
# The answer:
# Chef       Thelma
# Guard      Roberta
# Nurse      Steve
# Clerk      Pete
# Police     Steve
# Teacher    Roberta
# Actor      Pete
# Boxer      Thelma
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("QF_FD")

# data
n = 4

persons = range(n)
[Roberta, Thelma, Steve, Pete] = persons

person_names = ["Roberta", "Thelma", "Steve", "Pete"] 

# variables
Jobs = makeIntVector(sol, "Jobs",2*n,0,n-1)
[chef, guard, nurse, clerk, police_officer, teacher, actor, boxer] = Jobs

Jobs_s = ["chef", "guard", "nurse", "clerk", "police_officer", "teacher", "actor", "boxer"]

# constraints

# Each person holds exactly two jobs.
global_cardinality_count(sol,range(n),Jobs,[2,2,2,2])

#  The job of nurse is held by a male.
sol.add(Or(nurse == Steve, nurse == Pete))

#  The husband of the chef is the clerk.
sol.add(Or(clerk == Steve, clerk == Pete))
sol.add(Or(chef == Roberta,chef == Thelma))
sol.add(chef != clerk)

#  Roberta is not a boxer.
sol.add(Roberta != boxer)

#  Pete has no education past the ninth grade.
sol.add(Pete != teacher,Pete != police_officer, Pete != nurse)

# Roberta, [and] the chef, and the police officer went golfing together.
sol.add(Roberta != chef, chef != police_officer, Roberta != police_officer)

# From the name of the job
sol.add(Or(actor == Steve, actor == Pete))

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print(["%s:%s" % (Jobs_s[i],person_names[mod.eval(Jobs[i]).as_long()])
         for i in range(2*n)])
  getDifferentSolution(sol,mod,Jobs)

print("num_solutions:", num_solutions)

