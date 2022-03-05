#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Ormat game in Z3
#
# From bit-player "The Ormat Game"
# http://bit-player.org/2010/the-ormat-game
# """
# I'm going to give you a square grid, with some of the cells colored 
# and others possibly left blank. We'll call this a template. Perhaps 
# the grid will be one of these 3x3 templates:
# 
# [see pictures at the web page]
# 
# You have a supply of transparent plastic overlays that match the 
# grid in size and shape and that also bear patterns of black dots:
# 
# [ibid.]
#
# Your task is to assemble a subset of the overlays and lay them on 
# the template in such a way that dots cover all the colored squares 
# but none of the blank squares. You are welcome to superimpose multiple 
# dots on any colored square, but overall you want to use as few overlays 
# as possible. To make things interesting, I'll suggest a wager. I'll pay 
# you $3 for a correct covering of a 3x3 template, but you have to pay me 
# $1 for each overlay you use. Is this a good bet?
# """
# 
# This model consists of
# - generating the n! possible overlays to select from
# - it just shows which overlays that is used. It would be nice
#   with a much more graphical output.
# - the questions asked by bit-player is not answered (it requires
#   more analysis)
#
# That said, here is solutions of the three problems stated at the web page
# with minimum number of overlays.
#    x is an indicator matrix which overlay to use
#    overlay used is a set representation of the overlay used
#    num_overlays is the number of overlays used
# 
# Problem 1 (unique optimal solution)
#     x: [1 1 0 0 0 0] 
#     overlays_used: 1..2
#     num_overlays: 2
#
# Problem 2 (two optimal solutions)
#     x: [1 0 0 1 1 0] 
#     overlays_used: 1,4,5
#     num_overlays: 3
# 
#     alternative solution
#     x: [0 1 1 0 0 1]
#     overlays_used = 2,3,6
#     num_overlays = 3
#
# Problem 3 (unique optimal solution)
#     x: [0 1 0 1 1 1] 
#     overlays_used: 2,4,5,6
#     num_overlays: 4
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *
import random

def prod(n):
    return reduce(lambda x, y: x * y, range(1,n+1))


#
# Test all possible problems of size <n>.
# How many of them are solveable, i.e. if ormat_game() returns > 0 solutions.
#
# Number of solvable problems for som (small) n
# n   possible problems   solvable problems
# -----------------------------------------
# 3       512                 49
# 4     65536               7443
# 5  33554432
#
# (Number of possible problems is 2**(n*n)
#
#
def test_all_problems(n):
    sol = Solver()

    x = {}
    for i in range(n):
      for j in range(n):
        x[(i,j)] = makeIntVar(sol,"x[%i,%i]" % (i,j),0,1)

    num_solutions = 0
    overlays = []
    num_problems = 0
    num_solvable = 0
    while sol.check() == sat:
      num_problems += 1
      mod = sol.model()
      xx = [[mod.eval(x[(i,j)]).as_long() for j in range(n)] for i in range(n)]
      solvable = ormat_game(["problem", n,xx],0)
      # print(num_problems, "solvable: ", solvable)
      if solvable > 0:
        num_solvable += 1
        print("x: ", xx, " number of solvable: %i total number of problems: %i so far:" % (num_solvable, num_problems))
      getDifferentSolutionMatrix(sol,mod,x,n,n)

    print("n:", n)
    print("num_problems:", num_problems)
    print("num_solvable:", num_solvable, " (%2f%%)" % (num_solvable / num_problems))
        
    

#
# generate the n! overlays
# Note: This the unoptimized version.
#
# Better use generate_overlays2(n,problem) instead.
#
def generate_overlays(n): # Don't use!
    
    sol = Solver()

    x = {}
    for i in range(n):
      for j in range(n):
        x[(i,j)] = makeIntVar(sol,"x[%i,%i]" % (i,j),0,1)
    for i in range(n):
      sol.add(Sum([x[(i,j)] for j in range(n)]) == 1)
      sol.add(Sum([x[(j,i)] for j in range(n)]) == 1)

    num_solutions = 0
    overlays = []
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        overlays.append([[mod.eval(x[(i,j)]).as_long() for j in range(n)] for i in range(n)])
        getDifferentSolutionMatrix(sol,mod,x,n,n)

    return overlays


#
# generate the << n! overlays
#
# This version checks that it is a possible overlay,
# i.e. that is: when then problem has an 0 in a cell
# then the overlay also must have an 0 in the same cell.
# This reduces the number of overlays and the complexity 
# of the problem.
# Note: The reduction percetage is thus problem specific.
#
def generate_overlays2(n,problem):
    
    sol = Solver()

    x = {}
    for i in range(n):
      for j in range(n):
        x[(i,j)] = makeIntVar(sol,"x[%i,%i]" % (i,j),0,1)
    for i in range(n):
      for j in range(n):
        sol.add(If(problem[i][j] == 0, x[(i,j)] == 0, True))
      sol.add(Sum([x[(i,j)] for j in range(n)]) == 1)
      sol.add(Sum([x[(j,i)] for j in range(n)]) == 1)

    num_solutions = 0
    overlays = []
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        overlays.append([[mod.eval(x[(i,j)]).as_long() for j in range(n)] for i in range(n)])
        getDifferentSolutionMatrix(sol,mod,x,n,n)

    return overlays

    

#
# solve the Ormat game
#
def ormat_game(game,printit=1):

    # data
    
    name = game[0]
    n = game[1]
    problem = game[2]
    overlays = generate_overlays2(n,problem)
    # f = prod(n)
    f = len(overlays)

    if printit == 1:
      print(name, "\n")
      print(f, "overlays generated")
      print("problem:\n",end=" ")

      for i in range(n):
        for j in range(n):
          print(problem[i][j],end=" ")
        print()
      print()
    
    sol = SolverFor("LIA")

    # variables
    x = makeIntVector(sol,"x",f,0,1)
    num_overlays = makeIntVar(sol,"num_overlays",1,f)

    # constraints
    sol.add(num_overlays == Sum(x))
    for i in range(n):
        for j in range(n):
            if problem[i][j] == 1:
                sol.add(Sum([x[o]*overlays[o][i][j] for o in range(f)]) >= 1)
            else:
                sol.add(Sum([x[o]*overlays[o][i][j] for o in range(f)]) == 0)

    num_solutions = 0
    while sol.check() == sat:
        if printit == 1:
          print("\nsolution #%i" % num_solutions)
        num_solutions += 1
        mod = sol.model()
        if printit == 1:
          print("num_overlays:", mod.eval(num_overlays))
          # print("x :", [mod.eval(x[o]) for o in range(f)])
          selected = [o for o in range(f) if mod.eval(x[o]).as_long() == 1 ]
          print("overlay used: ", selected)
          for o in selected:
            xo = mod.eval(x[o]).as_long()
            print("\noverlay #%i" % o)
            for i in range(n):
              for j in range(n):
                print(overlays[o][i][j],end=" ")
              print()
          print("\nnum_overlays:", mod.eval(num_overlays))
          print()
        # getDifferentSolution(sol,mod,x)
        getLessSolution(sol,mod,num_overlays)

    if printit == 1:
      print("num_solutions:", num_solutions)
    return num_solutions


# Problem 1
# 0.4s
problem1 = ["problem1",3,
            [[1,0,0],
             [0,1,1],
             [0,1,1]]]

# Problem 2
# 0.4s
problem2 = ["problem2",3,
            [[1,1,1],
             [1,1,1],
             [1,1,1]]]

# 0.4s
problem3 = ["problem3", 3,
            [[1,1,1],
             [1,1,1],
             [1,1,0]]]

# 0.45s
problem4 = ["problem4",4,
            [[1,1,1,1],
             [1,1,1,1],
             [1,1,1,1],
             [1,1,0,0]]]

# 0.7s
problem5 = ["problem5",5,
            [[1,1,1,1,1],
             [1,1,1,1,1],
             [1,1,1,1,1],
             [1,1,1,1,1],
             [1,1,0,0,0]]]

# 2.4s
problem6 = ["problem6",6,
            [[1,1,1,1,1,1],
             [1,1,1,1,1,1],
             [1,1,1,1,1,1],
             [1,1,1,1,1,1],
             [1,1,1,1,1,1],
             [1,1,0,0,0,0]]]

# 1.2s
problem7 = ["problem7",7,
            [[1,1,1,1,1,1,1],
             [1,1,1,1,1,1,1],
             [0,1,1,1,1,1,1],
             [0,0,1,1,1,1,1],
             [0,0,0,1,1,1,1],
             [0,0,0,0,1,1,1],
             [0,0,0,0,0,1,1]]]

# 7.3s
problem8 = ["problem8",7,
            [[0,0,0,1,1,1,1],
             [0,0,0,1,1,1,1],
             [0,0,0,1,1,1,1],
             [1,1,1,1,1,1,1],
             [1,1,1,1,1,1,1],
             [1,1,1,1,1,1,1],
             [1,1,1,1,1,1,1]]]


# Syntax: ormat_game <n> <s>
# if only n: solve problem<n>
# if <s>: generate a random problem matrix of size n with seed <s>
#         Note: may not give any solution.
#         if s == -1, no seed is given

# Default problem
n = 3 # problem number
problem = problem8
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
    problem = eval("problem%i" % n)
  if len(sys.argv) > 2:
    s = int(sys.argv[2])
    if s >= 0:
        random.seed(s)
    gen = [[random.randint(0,1) for j in range(n)] for i in range(n)]
    problem = ["random problem",n,gen]
  
  ormat_game(problem,1)
  # test_all_problems(3)

