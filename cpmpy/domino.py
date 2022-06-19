"""
Domino problem in cpmpy.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


#
# Exactly one of A[I,J]'s neighbors is the same as A[I,J]
#
def form_domino(a,i,j,r,c):
    bs = []
    for ii in range(i-1,i+2):
        for jj in range(j-1,j+2):
            if ii >= 0 and ii < r and jj >= 0 and jj < c and (ii == i or jj == j) and [i,j] != [ii,jj]:
                bs.append(a[ii,jj] == a[i,j])
    return [sum(bs) == 1]
              

#
# Gecode presentation
#
# For the 7x8 problem (puzzl1, the Dell puzzle)
# Example:
# 
#   Pieces:
#   10 10  9 27 22  2 14 14
#   20 20  9 27 22  2 21  7
#   26 28 28  8 16 16 21  7
#   26 13  5  8 19  4  4  1
#   25 13  5 18 19 15 24  1
#   25 12 12 18 23 15 24  6
#   11 11  3  3 23 17 17  6
#
#   Gecode's representation:
#   998QL1DD
#   JJ8QL1K6
#   PRR7FFK6
#   PC47I330
#   OC4HIEN0
#   OBBHMEN5
#   AA22MGG5
#
def print_board2(a,d,rows,cols,dmap):
  print("\nAnother representation:")
  n = len(dmap)
  ddd = {t:i for (t,i) in zip([v[0] for v in dmap],range(n))}
  s = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  for i in range(rows):
    for j in range(cols):
        for ix in range(len(d)):
            if d[ix] == a[i,j].value(): 
                # print("%3s (%s)" % (d[ix],dmap[ix]),end=" ")
                v,ii,jj = dmap[ix]
                print("%2d-%2d" % (ii,jj) ,end=" ")                
    print()
    
  # Can only manage dmaps of size <= 62, e.g. not the sicstus instance).
  if n < len(s):
      print("\nAnd yet another representation:")
      for i in range(rows):
          for j in range(cols):
              print(s[ddd[a[i,j].value()]],end="")
          print()
      print()
  print()

def domino(problem,num_sols=1):

    model = Model()

    #
    # data
    #
    rows = len(problem)
    cols = len(problem[0])
    print("rows:",rows, "cols:",cols)
    
    max_val = max([problem[i][j] for i in range(rows) for j in range(cols)])
    
    # To handle larger instances (i.e. max val > 9)
    # we have to tweak a little
    mod_val = 10
    if max_val < 10:
        mod_val = 10
    else:
        mod_val = max_val + 1

    #
    # Convert each domino piece <a,b> to one number "ab"
    #
    # D=[0,1,6,11,..,16,...,55,56,66]
    dmap = [(i*mod_val+j,i,j) for i in range(max_val+1) for j in range(i,max_val+1)]
    d = [i*mod_val+j for i in range(max_val+1) for j in range(i,max_val+1)]
    min_dom = min(d)
    max_dom = max(d)
    
    # declare variables
    a = intvar(min_dom,max_dom,shape=(rows,cols),name="a")
    a_flat = a.flat

    # Restrict to the values in the domain (d)
    # This is not needed since we restrict the values in
    # the "cardinality count" below.
    # Also, it yield some strange error:
    # AttributeError: 'numpy.bool_' object has no attribute 'is_bool'
    # for i in range(rows):
    #     for j in range(cols):
    #         model += [member_of(d,a[i,j])]
    
    #
    # constraints
    #
    # All possible combinations
    table_accept = [(e,e % mod_val) for e in d ] + [(e,e // mod_val) for e in d]

    for i in range(rows):
        for j in range(cols):
            if problem[i][j] >= 0:
                model += [Table((a[i,j],problem[i][j]), table_accept)]
            model += [form_domino(a,i,j,rows,cols)]


    # Must have exactly 2 occurrences of each number in the domain
    for val in d:
        model += [count(a_flat,val,2)]

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    def print_sol():
        print(a.value())
        print_board2(a,d,rows,cols,dmap)
        print()

    num_solutions = ss.solveAll(solution_limit=num_sols, display=print_sol)
    print("num_solutions:", num_solutions)
                         


#
# The Dell Logic Puzzle example from the B-Prolog model
# 1 solution:
#
#  13  13  12  56  36   1  22  22 
#  34  34  12  56  36   1  35   6 
#  55  66  66  11  24  24  35   6 
#  55  16   4  11  33   3   3   0 
#  46  16   4  26  33  23  45   0 
#  46  15  15  26  44  23  45   5 
#  14  14   2   2  44  25  25   5 
#
# 1- 3  1- 3  1- 2  5- 6  3- 6  0- 1  2- 2  2- 2 
# 3- 4  3- 4  1- 2  5- 6  3- 6  0- 1  3- 5  0- 6 
# 5- 5  6- 6  6- 6  1- 1  2- 4  2- 4  3- 5  0- 6 
# 5- 5  1- 6  0- 4  1- 1  3- 3  0- 3  0- 3  0- 0 
# 4- 6  1- 6  0- 4  2- 6  3- 3  2- 3  4- 5  0- 0 
# 4- 6  1- 5  1- 5  2- 6  4- 4  2- 3  4- 5  0- 5 
# 1- 4  1- 4  0- 2  0- 2  4- 4  2- 5  2- 5  0- 5 
#
#
# NumSolutions: 1
#
puzzle1 = [[3,1,2,6,6,1,2,2],
          [3,4,1,5,3,0,3,6],
          [5,6,6,1,2,4,5,0],
          [5,6,4,1,3,3,0,0],
          [6,1,0,6,3,2,4,0],
          [4,1,5,2,4,3,5,5],
          [4,1,0,2,4,5,2,0]]


#
# We need at least one hint with the largest possible value.
# This has - of course - a huge number of solutions.
#
M = -1
puzzle2 = [[M,M,M,6,M,M,M,M],
          [M,M,M,M,M,M,M,M],
          [M,M,M,M,M,M,M,M],
          [M,M,M,M,M,M,M,M],
          [M,M,M,M,M,M,M,M],
          [M,M,M,M,M,M,M,M],
          [M,M,M,M,M,M,M,M]]

#
# From SICStus example dominoes.pl (instance 111)
# 12 solutions
#
#  ....
#
# Another representation:
# 1- 7  4- 4  4- 4  2- 8  2- 8  8- 9  2- 3  2- 3  0- 9 10-11  1-10  1-10  0- 7 
# 1- 7  0- 2  0- 2  8- 8  8- 8  8- 9  0-11  0-11  0- 9 10-11  1- 5  2- 7  0- 7 
# 3- 9  3- 9  2- 2  9-10  0- 0  1- 3  1- 3  2-11  1- 8  1- 8  1- 5  2- 7  7- 7 
# 0- 1  4-11  2- 2  9-10  0- 0  8-11  7- 9  2-11  1-11  1-11  0-10  1- 1  7- 7 
# 0- 1  4-11  2- 9  6- 9  6- 9  8-11  7- 9  5- 9  5- 9  5- 6  0-10  1- 1  6- 7 
# 4-10  4-10  2- 9  6- 8  6- 8  4- 5  4- 5  6-10  6-10  5- 6  2- 5  7- 8  6- 7 
# 4- 6  2- 6  2- 6  6- 6  6- 6  0- 6  0- 6  3- 7  0- 8  2-10  2- 5  7- 8  3-11 
# 4- 6  9- 9  0- 5  0- 3  0- 3  7-10  7-10  3- 7  0- 8  2-10  3- 8  8-10  3-11 
# 1- 4  9- 9  0- 5 10-10  5- 7  5- 7  4- 8  4- 8  4- 9  7-11  3- 8  8-10  9-11 
# 1- 4  0- 4  0- 4 10-10 11-11 11-11  2- 4  3-10  4- 9  7-11  5- 5  5- 5  9-11 
# 3- 6  3- 6  1- 2  1- 2  1- 6  5-10  2- 4  3-10  3- 3  5- 8  6-11  3- 5  4- 7 
# 1- 9  1- 9  3- 4  3- 4  1- 6  5-10  5-11  5-11  3- 3  5- 8  6-11  3- 5  4- 7 
#
# NumSolutions: 12
# 
sicstus111 = [
        [1,4,4,8,2,8,3,2,9,11,10,1,7],
        [7,0,2,8,8,9,11,0,0,10,5,2,0],
        [3,9,2,9,0,1,3,11,8,1,1,7,7],
        [1,4,2,10,0,8,7,2,1,11,0,1,7],
        [0,11,2,9,6,11,9,5,9,6,10,1,7],
        [10,4,9,8,6,5,4,6,10,5,2,7,6],
        [6,6,2,6,6,0,6,3,0,10,5,8,11],
        [4,9,5,3,0,10,7,7,8,2,3,10,3],
        [1,9,0,10,5,7,8,4,9,11,8,8,9],
        [4,4,0,10,11,11,2,10,4,7,5,5,11],
        [3,6,2,1,6,10,4,3,3,5,6,5,7],
        [1,9,3,4,1,5,11,5,3,8,11,3,4]

    ]

print("Puzzle1")
domino(puzzle1,0)
print("\nPuzzle2 (just a few of many solutions)")
domino(puzzle2,7) # many solutions
print("\nPuzzle sicstus 111")
domino(sicstus111,0)
