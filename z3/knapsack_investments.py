#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Knapsack (investment) problem in Z3
#
# From the Swedish book
#
# Lundgren, Rönnqvist, Värbrand "Optimeringslära" [Optimization Theory], page 393ff.
# 
# A company shall invest in some building projects with the following
# limits:
#
#  - budget of 225 Mkr (million swedish kronor)
#  - 28 persons available
#  - maximum 9 projects can be selected
#  - some project may not be selected together with other projects, and some
#    projects must be selected together with other.
# 
# (I've kept the Swedish object names.)
#
# No.  Object   Value(kkr) Budget(Mkr) Personell  Not with  Requires
# 1  Ishall      600        35            5        10        -
# 2  Sporthall   400        34            3        -         -
# 3  Hotell      100        26            4        -         15
# 4  Restaurang  150        12            2        -         15
# 5  Kontor A     80        10            2        6         -
# 6  Kontor B    120        18            2        5         -
# 7  Skola       200        32            4        -         -
# 8  Dagis       220        11            1        -         7
# 9  Lager        90        10            1        -         -
# 10 Simhall     380        22            5        1         -
# 11 Hyreshus    290        27            3        15        -
# 12 Bilverkstad 130        18            2        -         -
# 13 Tennishall   80        16            2        -         2
# 14 Idrottsanl. 270        29            4        -         2
# 15 Båthamn     280        22            3        11        -
# 
#
# Solution (page 395): 
# The following project is selected
#   1,2,4,6,7,8,12,14,15
# and optimal value is 2370kkr.
#
#
# This model uses a more general model than the book's model.
#
# The solution is the same as the book (well, we must check,
# mustn't we? :-)
#
# x = [1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1]
#      1  2     4     6  7  8          12    14 15
# total_values = 2370
# total_projects = 9
# total_persons = 26
# total_budget = 211
#
# Question: Is there another solution with total_values = 2370?
# Change to solve satisfy and test...
# Answer: No, that's the unique solution.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

num_projects = 15 # number of projects to select from
max_budget = 225 # budget limit 
max_projects = 9 # max number of projects to select
max_persons = 28 # persons available
# the values and budgets of each project
values = [600,400,100,150, 80,120,200,220, 90,380,290,130, 80,270,280]
budgets = [35,34,26,12,10,18,32,11,10,22,27,18,16,29,22]

# project i cannot be selected with project j
num_not_with = 6
not_with = [ 
  [1, 10],
  [5, 6],
  [6, 5],
  [10, 1],
  [11, 15],
  [15, 11]
]

# project i requires project j 
num_requires = 5
requires = [
  [3, 15],
  [4, 15],
  [8, 7],
  [13, 2],
  [14, 2]
]

personell = [5,3,4,2,2,2,4,1,1,5,3,2,2,4,3]

#
# decision variable
#

# what project to select
x = makeIntVector(sol, "x", num_projects, 0,1)

total_persons = makeIntVar(sol, "total_persons", 0, max_persons)
total_budget  = makeIntVar(sol, "total_budget", 0, max_budget)
total_projects = makeIntVar(sol, "total_project", 0, max_budget)

# the objective to maximize
total_values  = makeIntVar(sol, "total_values", 0, sum(values))

# solve maximize total_values;

sol.add(total_persons  == Sum([x[i]*personell[i] for i in range(num_projects)]))
sol.add(total_budget   == Sum([x[i]*budgets[i] for i in range(num_projects)]))
sol.add(total_projects == Sum([x[i] for i in range(num_projects)]))
sol.add(total_values   == Sum([x[i]*values[i] for i in range(num_projects)]))

# resource limits:
sol.add(total_budget <= max_budget)
sol.add(total_persons <= max_persons)
sol.add(total_projects <= max_projects)

#
# special requirements, using standard integer programming "tricks"
#
# projects that require other projects
for i in range(num_requires):
   sol.add(x[requires[i][0]-1] - x[requires[i][1]-1] <= 0)


# projects excluding other projects
for i in range(num_not_with):
   sol.add(x[not_with[i][0]-1] + x[not_with[i][1]-1] <= 1)

# Question: Is there another solution with total_values = 2370?
# sol.add(total_values == 2370) # (change to getDifferentSolution)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  # print("x:", [mod.eval(x[i]) for i in range(num_projects)])
  print("x:", [i+1 for i in range(num_projects) if mod.eval(x[i]).as_long() == 1])
  print("total_values: ", mod.eval(total_values))
  print("total_projects: ", mod.eval(total_projects))
  print("total_persons: ", mod.eval(total_persons))
  print("total_budget: ", mod.eval(total_budget))
  print()
  getGreaterSolution(sol,mod,total_values)
  # getDifferentSolution(sol,mod,x) # for checking unicity of the optimal solution

print("num_solutions:", num_solutions)




