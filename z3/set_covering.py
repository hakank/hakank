#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Set covering in Z3
#
# Some set covering problems. See below for descriptions.
# - Placing of firestations, from Winston 'Operations Research'
# - Minimize the number of security telephones in street corners on a campus. (Taha)
# - Senators making a committee (Katta G Murty)
# - Set partition and set covering of alternatives (Lundgren, Roennqvist, Vaerbrand)
# - Smallest subset problem (Steven Skiena)
# - Knuth's exact set covering problem
# - Set covering deployment
# - Set covering from OPL
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *

#
# Placing of firestations, from Winston 'Operations Research', page 486.
#
def set_covering1(problem):
  print(problem)

  # sol = Solver()
  sol = Optimize()

  # data
  min_distance = 15
  num_cities = 6

  distance = [
      [0, 10, 20, 30, 30, 20],
      [10, 0, 25, 35, 20, 10],
      [20, 25, 0, 15, 30, 20],
      [30, 35, 15, 0, 15, 25],
      [30, 20, 30, 15, 0, 14],
      [20, 10, 20, 25, 14, 0]
  ]

  # declare variables
  # x = [solver.IntVar(0, 1, "x[%i]" % i) for i in range(num_cities)]
  x = makeIntVars(sol, "x", num_cities, 0, 1)

  #
  # constraints
  #

  # objective to minimize
  z = makeIntVar(sol, "z", 0,num_cities)
  sol.add(z == Sum([x[i] for i in range(num_cities)]))

  # ensure that all cities are covered (inside the min_distance neighbourhood)
  for i in range(num_cities):
    sol.add(Sum([x[j] for j in range(num_cities) if distance[i][j] <= min_distance]) >= 1)

  sol.minimize(z)

  #
  # solution and search
  #
  if sol.check() == sat:
    mod = sol.model()
    print("z:", mod.eval(z))
    print("x:", [mod.eval(x[i]) for i in range(num_cities)])

#
#  Example 9.1-2, page 354ff, from
#  Taha 'Operations Research - An Introduction'
#  Minimize the number of security telephones in street corners on a campus.
#
def set_covering2(problem):

  print(problem)

  sol = Optimize()

  # data
  n = 8  # maximum number of corners
  num_streets = 11  # number of connected streets

  # corners of each street
  # Note: 1-based (handled below)
  corner = [
      [1, 2],
      [2, 3],
      [4, 5],
      [7, 8],
      [6, 7],
      [2, 6],
      [1, 6],
      [4, 7],
      [2, 4],
      [5, 8],
      [3, 5]
  ]

  #
  # declare variables
  #
  # [solver.IntVar(0, 1, "x[%i]" % i) for i in range(n)]
  x = makeIntVars(sol, "x", n, 0, 1)

  # constraints

  # number of telephones, to be minimized
  z = makeIntVar(sol,"z",0,n)
  sol.add(z == Sum([x[i] for i in range(n)]))

  # ensure that all corners are covered
  for i in range(num_streets):
    # also, convert to 0-based
    # sol.add(solver.SumGreaterOrEqual([x[j - 1] for j in corner[i]], 1))
    sol.add(Sum([x[j-1] for j in corner[i]]) >= 1)

  sol.minimize(z)

  # solution and search
  if sol.check() == sat:
    mod = sol.model()
    print("z:", mod.eval(z))
    print("x:", [mod.eval(x[i]) for i in range(n)])


#
# Problem from
# Katta G. Murty: 'Optimization Models for Decision Making', page 302f
# http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
#
# 10 senators making a committee, where there must at least be one
# representative from each group:
# group:        senators:
# southern      1 2 3 4 5
# northern      6 7 8 9 10
# liberals      2 3 8 9 10
# conservative  1 5 6 7
# democrats     3 4 5 6 7 9
# republicans   1 2 8 10
#
# The objective is to minimize the number of senators.
#
def set_covering3(problem):
  print(problem)
        
  sol = Optimize()

  # data
  num_groups = 6
  num_senators = 10

  # which group does a senator belong to?
  belongs = [
      [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],   # 1 southern
      [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],   # 2 northern
      [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],   # 3 liberals
      [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],   # 4 conservative
      [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],   # 5 democrats
      [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]    # 6 republicans
  ]

  # declare variables
  x = makeIntVars(sol, "x",num_senators,0,1)

  # constraints

  # number of assigned senators (to minimize)
  z = makeIntVar(sol,"z",0,num_senators)
  sol.add(z == Sum([x[i] for i in range(num_senators)]))

  # ensure that each group is covered by at least
  # one senator
  for i in range(num_groups):
    sol.add(
        Sum([x[j]*belongs[i][j] for j in range(num_senators)]) >= 1)

  sol.minimize(z)

  # solution and search
  if sol.check() == sat:
    mod = sol.model()
    print("z:", mod.eval(z))
    print("x:", [mod.eval(x[i]) for i in range(num_senators)])
    for j in range(num_senators):
        if mod.eval(x[j]).as_long() == 1:
          print("Senator", j + 1, "belongs to these groups:", end=' ')
          for i in range(num_groups):
            if belongs[i][j] == 1:
              print(i + 1, end=' ')
          print()


#
# Set partition and set covering
#
# Example from the Swedish book
# Lundgren, Roennqvist, Vaebrand
# 'Optimeringslaera' (translation: 'Optimization theory'),
# page 408.
#
# * Set partition:
#   We want to minimize the cost of the alternatives which covers all the
#   objects, i.e. all objects must be choosen. The requirement is than an
#   object may be selected _exactly_ once.
#
#   Note: This is 1-based representation
#
#   Alternative        Cost        Object
#   1                  19           1,6
#   2                  16           2,6,8
#   3                  18           1,4,7
#   4                  13           2,3,5
#   5                  15           2,5
#   6                  19           2,3
#   7                  15           2,3,4
#   8                  17           4,5,8
#   9                  16           3,6,8
#   10                 15           1,6,7
#
#   The problem has a unique solution of z = 49 where alternatives
#        3, 5, and 9
#   is selected.
#
# * Set covering:
#   If we, however, allow that an object is selected _more than one time_,
#   then the solution is z = 45 (i.e. less cost than the first problem),
#   and the alternatives
#        4, 8, and 10
#   is selected, where object 5 is selected twice (alt. 4 and 8).
#   It's an unique solution as well.

def set_covering4(problem, set_partition=1):
  print(problem)

  sol = Optimize()

  #
  # data
  #
  num_alternatives = 10
  num_objects = 8

  # costs for the alternatives
  costs = [19, 16, 18, 13, 15, 19, 15, 17, 16, 15]

  # the alternatives, and their objects
  a = [
      # 1 2 3 4 5 6 7 8    the objects
      [1, 0, 0, 0, 0, 1, 0, 0],  # alternative 1
      [0, 1, 0, 0, 0, 1, 0, 1],  # alternative 2
      [1, 0, 0, 1, 0, 0, 1, 0],  # alternative 3
      [0, 1, 1, 0, 1, 0, 0, 0],  # alternative 4
      [0, 1, 0, 0, 1, 0, 0, 0],  # alternative 5
      [0, 1, 1, 0, 0, 0, 0, 0],  # alternative 6
      [0, 1, 1, 1, 0, 0, 0, 0],  # alternative 7
      [0, 0, 0, 1, 1, 0, 0, 1],  # alternative 8
      [0, 0, 1, 0, 0, 1, 0, 1],  # alternative 9
      [1, 0, 0, 0, 0, 1, 1, 0]   # alternative 10
  ]

  # declare variables
  x = makeIntVars(sol,"x",num_alternatives,0,1)

  # constraints

  # sum the cost of the choosen alternative,
  # to be minimized
  z = makeIntVar(sol,"z",0,100)
  scalar_product(sol, costs, x, z)

  #
  for j in range(num_objects):
    if set_partition == 1:
      sol.add(Sum([x[i] * a[i][j] for i in range(num_alternatives)]) == 1)
    else:
      sol.add(Sum([x[i] * a[i][j] for i in range(num_alternatives)]) >= 1)        

  sol.minimize(z)

  # solution and search
  if sol.check() == sat:
    mod = sol.model()
    print("z:", mod.eval(z))
    print("selected alternatives:", [i + 1 for i in range(num_alternatives)
                                   if mod.eval(x[i]).as_long() == 1])


#
# Example from Steven Skiena, The Stony Brook Algorithm Repository
# http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
# '''
# Input Description: A set of subsets S_1, ..., S_m of the
# universal set U = {1,...,n}.
#
# Problem: What is the smallest subset of subsets T subset S such
# that \cup_{t_i in T} t_i = U?
# '''
# Data is from the pictures INPUT/OUTPUT.
#
def set_covering_skiena(problem):
  print(problem)

  sol = Optimize()

  # data
  num_sets = 7
  num_elements = 12
  belongs = [
      # 1 2 3 4 5 6 7 8 9 0 1 2  elements
      [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],  # Set 1
      [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],  # 2
      [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0],  # 3
      [0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0],  # 4
      [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],  # 5
      [1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0],  # 6
      [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1]  # 7
  ]

  # variables
  x = makeIntVars(sol,"x",num_sets,0,1) 

  # number of choosen sets
  z = makeIntVar(sol,"z",0,num_sets*2)

  # total number of elements in the choosen sets
  tot_elements = makeIntVar(sol,"tot_elements",0,num_sets*num_elements)

  #
  # constraints
  #
  sol.add(z == Sum([x[i] for i in range(num_sets) ]))

  # all sets must be used
  for j in range(num_elements):
    sol.add(Sum([belongs[i][j] * x[i] for i in range(num_sets)]) >= 1)

  # number of used elements
  sol.add(tot_elements == Sum([x[i] * belongs[i][j]
                               for i in range(num_sets)
                               for j in range(num_elements)]))

  # objective
  sol.minimize(z)

  # search and result
  if sol.check() == sat:
    mod = sol.model()
    print('z:', mod.eval(z))
    print('tot_elements:', mod.eval(tot_elements))
    print('x:', [mod.eval(x[i]) for i in range(num_sets)])

#
# Knuth's exact set covering problem
# From
# http://en.wikipedia.org/wiki/Algorithm_X 
# 
# """
# Donald Knuth's Algorithm X is a recursive, nondeterministic, depth-first, 
# backtracking algorithm that finds all solutions to the _exact cover_ problem 
# represented by a matrix A consisting of 0s and 1s. The goal is to select a 
# subset of the rows so that the digit 1 appears in each column exactly once.
# ...
# 
# For example, consider the exact cover problem specified by the universe 
#  U = {1, 2, 3, 4, 5, 6, 7} and the collection of sets 
#  S = {A, B, C, D, E, F}, where:
#
#    A = {1, 4, 7};
#    B = {1, 4};
#    C = {4, 5, 7};
#    D = {3, 5, 6};
#    E = {2, 3, 6, 7}; and
#    F = {2, 7}
# 
# ...
# In summary, the algorithm determines there is only one exact cover: 
#   S = {B, D, F}
# """
#
# Also see:
# * http://en.wikipedia.org/wiki/Exact_cover
#
def set_covering_knuth(problem):
  print(problem)

  sol = Optimize()

  # data
  num_sets = 6
  num_elements = 7
  belongs = [
     # 1  2  3  4  5  6  7  elements
      [1, 0, 0, 1, 0, 0, 1],  # A
      [1, 0, 0, 1, 0, 0, 0],  # B
      [0, 0, 0, 1, 1, 0, 1],  # C
      [0, 0, 1, 0, 1, 1, 0],  # D
      [0, 1, 1, 0, 0, 1, 1],  # E
      [0, 1, 0, 0, 0, 0, 1],  # F
  ]

  # variables
  x = makeIntVars(sol,"x",num_sets,0,1) 

  # number of choosen sets
  z = makeIntVar(sol,"z",0,num_sets*2)

  #
  # constraints
  #
  sol.add(z == Sum([x[i] for i in range(num_sets) ]))

  # all sets must be used
  for j in range(num_elements):
    sol.add(Sum([belongs[i][j] * x[i] for i in range(num_sets)]) >= 1)

  # objective
  sol.minimize(z)

  # search and result
  if sol.check() == sat:
    mod = sol.model()
    print('z:', mod.eval(z))
    print('x:', [mod.eval(x[i]) for i in range(num_sets)])

#
# Set covering deployment
#
# From http://mathworld.wolfram.com/SetCoveringDeployment.html
# '''
# Set covering deployment (sometimes written 'set-covering deployment'
# and abbreviated SCDP for 'set covering deployment problem') seeks
# an optimal stationing of troops in a set of regions so that a
# relatively small number of troop units can control a large
# geographic region. ReVelle and Rosing (2000) first described
# this in a study of Emperor Constantine the Great's mobile field
# army placements to secure the Roman Empire.
# '''
#
def set_covering_deployment(problem):
  print(problem)

  sol = Optimize()

  # data
  countries = ["Alexandria",
               "Asia Minor",
               "Britain",
               "Byzantium",
               "Gaul",
               "Iberia",
               "Rome",
               "Tunis"]
  n = len(countries)

  # the incidence matrix (neighbours)
  mat = [
      [0, 1, 0, 1, 0, 0, 1, 1],
      [1, 0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 1, 0, 0],
      [1, 1, 0, 0, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 1, 1, 0],
      [0, 0, 1, 0, 1, 0, 1, 1],
      [1, 0, 0, 1, 1, 1, 0, 1],
      [1, 0, 0, 0, 0, 1, 1, 0]
  ]

  # declare variables

  # First army
  X = makeIntVars(sol,"X",n,0,1)

  # Second (reserv) army
  Y = makeIntVars(sol,"Y",n,0,1)  

  # constraints

  # total number of armies
  num_armies = makeIntVar(sol,"num_armies", 0, 2*n)
  sol.add(num_armies == Sum([X[i] + Y[i] for i in range(n)]))

  #
  #  Constraint 1: There is always an army in a city
  #                (+ maybe a backup)
  #                Or rather: Is there a backup, there
  #                must be an an army
  #
  [sol.add(X[i] >= Y[i]) for i in range(n)]

  #
  # Constraint 2: There should always be an backup army near every city
  #
  for i in range(n):
    # neighbors = solver.Sum([Y[j] for j in range(n) if mat[i][j] == 1])
    # solver.Add(X[i] + neighbors >= 1)
    sol.add(X[i] + Sum([Y[j] for j in range(n) if mat[i][j] == 1]) >= 1)

  sol.minimize(num_armies)

  # solution and search
  if sol.check() == sat:
    mod = sol.model()
    print("num_armies:", mod.eval(num_armies))
    print("X:", [mod.eval(X[i]) for i in range(n)])
    print("Y:", [mod.eval(Y[i]) for i in range(n)])
    for i in range(n):
      if mod.eval(X[i]).as_long() == 1:
        print("army:", countries[i], end=' ')
      if mod.eval(Y[i]).as_long() == 1:
        print("reserv army:", countries[i], " ")
    print()


#
# set_covering_opl
#
# This example is from the OPL example covering.mod
# '''
# Consider selecting workers to build a house. The construction of a
# house can be divided into a number of tasks, each requiring a number of
# skills (e.g., plumbing or masonry). A worker may or may not perform a
# task, depending on skills. In addition, each worker can be hired for a
# cost that also depends on his qualifications. The problem consists of
# selecting a set of workers to perform all the tasks, while minimizing the
# cost. This is known as a set-covering problem. The key idea in modeling
# a set-covering problem as an integer program is to associate a 0/1
# variable with each worker to represent whether the worker is hired.
# To make sure that all the tasks are performed, it is sufficient to
# choose at least one worker by task. This constraint can be expressed by a
# simple linear inequality.
# '''

# Solution from the OPL model (1-based)
# '''
# Optimal solution found with objective: 14
# crew= {23 25 26}
# '''

# Solution from this model (0-based):
# '''
# Total cost 14
# We should hire these workers:  22 24 25
# '''
#
def set_covering_opl(problem):
  print(problem)
  
  sol = Optimize()

  # data
  nb_workers = 32
  Workers = list(range(nb_workers))
  num_tasks = 15
  Tasks = list(range(num_tasks))

  # Which worker is qualified for each task.
  # Note: This is 1-based and will be made 0-base below.
  Qualified = [
      [1, 9, 19, 22, 25, 28, 31],
      [2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32],
      [3, 10, 19, 24, 26, 30, 32],
      [4, 21, 25, 28, 32],
      [5, 11, 16, 22, 23, 27, 31],
      [6, 20, 24, 26, 30, 32],
      [7, 12, 17, 25, 30, 31],
      [8, 17, 20, 22, 23],
      [9, 13, 14, 26, 29, 30, 31],
      [10, 21, 25, 31, 32],
      [14, 15, 18, 23, 24, 27, 30, 32],
      [18, 19, 22, 24, 26, 29, 31],
      [11, 20, 25, 28, 30, 32],
      [16, 19, 23, 31],
      [9, 18, 26, 28, 31, 32]
  ]

  Cost = [
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5,
      5, 5, 6, 6, 6, 7, 8, 9]

  # variables
  Hire = [makeIntVar(sol, "Hire[%i]" % w, 0, 1) for w in Workers]
  total_cost = makeIntVar(sol, "total_cost", 0, nb_workers * sum(Cost))

  # constraints
  sol.add(total_cost == scalar_product2(sol,Hire, Cost))

  for j in Tasks:
    # Sum the cost for hiring the qualified workers
    # (also, make it 0-base)
    sol.add(Sum([Hire[c - 1] for c in Qualified[j]]) >= 1)

  sol.minimize(total_cost)

  if sol.check() == sat:
    mod = sol.model()
    print("Total cost", mod.eval(total_cost))
    print("We should hire these workers: ", end=' ')
    for w in Workers:
      if mod.eval(Hire[w]) == 1:
        print(w, end=' ')
    print()
  print()



if __name__ == "__main__":
  set_covering1("Set covering 1")
  print()
  set_covering2("Set covering 2")
  print()
  set_covering3("Set covering 3")
  print()
  set_covering4("Set covering 4 Set partition",1)
  print()
  set_covering4("Set covering 4 Set covering",0)
  print()
  set_covering_skiena("Set covering Skiena")
  print()
  set_covering_knuth("Set covering Knuth")
  print()
  set_covering_deployment("Set covering deployment")
  print()
  set_covering_opl("Set covering OPL")

