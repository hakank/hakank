#
# Sudoku Boolean encoding in z3.
#
# This Boolean model is inspired by GLPK:s MIP example sudoku.mod.
# (and was ported from the Picat model sudoku_ip.pi)
#
# QF_LIA Seems to be the fastest
# Here are the times for first solution (i.e. prove_unicity = False):
#  - world_hardest (9x9): 0.1963176727294922s
#  - another (9x9)      : 0.1930856704711914s
#  - problem_34 (16x16) : 1.0186767578125s
#  - problem_89 (25x25) : 3.8721377849578857s
#
# Here are the times for prove_unicity = True:
#  - world_hardest (9x9): 0.2767314910888672s
#  - another (9x9)      : 0.26558661460876465s
#  - problem_34 (16x16) : 1.4574153423309326s
#  - problem_89 (25x25) : 5.4920642375946045s
#
# QF_FD 
# Time to first solution:
#  - world_hardest (9x9): 0.19723749160766602s
#  - another (9x9)      : 0.19025015830993652s
#  - problem_34 (16x16) : 1.0626914501190186s
#  - problem_89 (25x25) : 4.13015604019165s
#
# Proving unicity:
#  - world_hardest (9x9): 0.2712392807006836s
#  - another (9x9)      : 0.26309657096862793
#  - problem_34 (16x16) : 1.448338508605957s
#  - problem_89 (25x25) : 5.640852451324463s
#
# Compare with sudoku.py which is faster on the simpler cases (9x9 and 16x16)
# but slower on the instance 89 (25x25):
# - 166.05s for finding first solution
# - 318.21s for proving unicity.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

import time, math
from z3 import *
from z3_utils_hakank import *

def sudoku_ip(init,prove_unicity=True):
    
    # sol = Solver()
    # sol = SimpleSolver()
    # sol = SolverFor("QF_LIA")
    # sol = SolverFor("QF_FD")
    sol = SolverFor("LIA")
        
    
    n = len(init)
    m = math.ceil(math.sqrt(n))
    line = list(range(n))
    # The values are 1..n
    k_line = list(range(1,n+1))
    ks = [i for i in range(0,n,m)]
        
    # Setup
    x = {}
    for i in line: 
        for j in line:
            for k in k_line:
                x[(i,j,k)] = Bool(f"x[{i},{j},{k}]")

    # Include the solution in the encoding (a little slower)
    # solution = {}
    # for i in line: 
    #     for j in line:
    #         solution[(i,j)] = Int(f"solution[{i},{j}]")
    #         sol.add(solution[(i,j)]>=1, solution[(i,j)]<= n)


    # Init grid
    for i in line:
        for j in line:
            if init[i][j] != 0:
                for k in k_line:
                    if init[i][j] == k:
                        sol.add(x[(i,j,k)] == True)


    # each cell must be assigned exactly one number 
    for i in line:
        for j in line:
            # sol.add(Sum([x[(i,j,k)] for k in line]) == 1)
            sol.add(AtLeast(*[x[(i,j,k)] for k in k_line],1))
            sol.add(AtMost(*[x[(i,j,k)] for k in k_line],1))


    # cells in the same row must be assigned distinct numbers 
    for i in line:
        for k in k_line:
            # sol.add(Sum([x[(i,j,k)] for j in line]) == 1)
            sol.add(AtLeast(*[x[(i,j,k)] for j in line],1))
            sol.add(AtMost(*[x[(i,j,k)] for j in line],1))


    # cells in the same column must be assigned distinct numbers 
    for j in line:
        for k in k_line:
          # sol.add(Sum([x[(i,j,k)] for i in line]) == 1)
          sol.add(AtLeast(*[x[(i,j,k)] for i in line],1))
          sol.add(AtMost(*[x[(i,j,k)] for i in line],1))

    # cells in the same region must be assigned distinct numbers 
    # foreach(I1 in Ks, J1 in Ks, K in 1..N)
    for i1 in ks:
        for j1 in ks:
          for k in k_line:
              # sol.add(Sum([x[(i,j,k)] for i in range(i1,i1+m) for j in range(j1,j1+m)]) == 1)
              sol.add(AtLeast(*[x[(i,j,k)] for i in range(i1,i1+m) for j in range(j1,j1+m)],1))
              sol.add(AtMost(*[x[(i,j,k)] for i in range(i1,i1+m) for j in range(j1,j1+m)],1))

    # Connect to solution
    # for i in line:
    #     for j in line:
    #         sol.add(solution[(i,j)] == sum([If(x[i,j,k],1,0) * k for k in k_line]))

    # Check for unicity
    if prove_unicity:
        while sol.check() == sat:
            mod = sol.model()
            # solution_res = [[mod[solution[(i,j)]] for j in line] for i in line]
            # print("solution:", solution_res )      
            print_solution(mod,x,init,line,k_line)
            x_flatten = [mod[x[(i,j,k)]] for i in line for j in line for k in k_line]
            # solution_flatten = [mod[solution_flatten[(i,j)]] for i in line for j in line]            
            getDifferentSolution(sol,mod,x_flatten)
    else:
        if sol.check() == sat:        
            mod = sol.model()
            # print("solution:", [[mod[solution[(i,j)]] for j in line] for i in line])
            print_solution(mod,x,init,line,k_line)


def print_solution(mod,x,init,line,k_line):
    for i in line: 
        for j in line:
            print(f"{sum([ k*(1 if mod[x[(i,j,k)]] else 0) for k in k_line]):2d}", flush=True, end=" ")
        print()
    print()
    

# Problem from
# "World's hardest sudoku: can you crack it?"
# http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
#
# Note: Null time (i.e. just reading this model w/o any goal): 0.204s
#
# 8 1 2 7 5 3 6 4 9 
# 9 4 3 6 8 2 1 7 5 
# 6 7 5 4 9 1 2 8 3 
# 1 5 4 2 3 7 8 9 6 
# 3 6 9 8 4 5 7 2 1 
# 2 8 7 1 6 9 5 3 4 
# 5 2 1 9 7 4 3 6 8 
# 4 3 8 5 2 6 9 1 7 
# 7 9 6 3 1 8 4 5 2 
#
world_hardest = [[8,0,0, 0,0,0, 0,0,0],
                 [0,0,3, 6,0,0, 0,0,0],
                 [0,7,0, 0,9,0, 2,0,0],

                 [0,5,0, 0,0,7, 0,0,0],
                 [0,0,0, 0,4,5, 7,0,0],
                 [0,0,0, 1,0,0, 0,3,0],

                 [0,0,1, 0,0,0, 0,6,8],
                 [0,0,8, 5,0,0, 0,1,0],
                 [0,9,0, 0,0,0, 4,0,0]]

# From https://ericpony.github.io/z3py-tutorial/guide-examples.htm
# (another Z3 Sudoku model)
# That z3 model solves this problem in 0.29s
# This model: 0.04s (0.06s to prove uniciy)
#
# 7 1 5 8 9 4 6 3 2 
# 2 3 4 5 1 6 8 9 7 
# 6 8 9 7 2 3 1 4 5 
# 4 9 3 6 5 7 2 1 8 
# 8 6 7 2 3 1 9 5 4 
# 1 5 2 4 8 9 7 6 3 
# 3 7 6 1 4 8 5 2 9 
# 9 2 8 3 6 5 4 7 1 
# 5 4 1 9 7 2 3 8 6 
#
another = [[0,0,0,0,9,4,0,3,0],
           [0,0,0,5,1,0,0,0,7],
           [0,8,9,0,0,0,0,4,0],
           [0,0,0,0,0,0,2,0,8],
           [0,6,0,2,0,1,0,5,0],
           [1,0,2,0,0,0,0,0,0],
           [0,7,0,0,0,0,5,2,0],
           [9,0,0,0,6,5,0,0,0],
           [0,4,0,9,7,0,0,0,0]]


# 
# This problem is problem 34 from
# Gecode's sudoku.cpp
# http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
#
# Size : 16 x 16
#
# problem: problem_34 16 x 16
# 13 9 2 11 15 12 10 1 16 6 14 7 4 3 8 5 
# 4 12 15 10 3 5 16 8 9 13 1 2 7 6 14 11 
# 3 14 7 1 4 6 2 13 15 5 8 11 12 9 16 10 
# 16 5 6 8 9 7 14 11 10 3 12 4 15 13 2 1 
# 12 7 16 5 10 8 11 15 3 2 6 1 14 4 9 13 
# 2 13 8 4 12 3 1 14 5 11 7 9 10 15 6 16 
# 1 11 10 14 2 9 6 4 13 15 16 8 3 7 5 12 
# 6 15 9 3 5 13 7 16 4 12 10 14 11 2 1 8 
# 15 3 14 16 1 2 9 5 7 8 11 10 6 12 13 4 
# 9 10 11 12 16 15 8 7 6 4 5 13 2 1 3 14 
# 5 8 4 2 13 10 3 6 14 1 9 12 16 11 7 15 
# 7 6 1 13 11 14 4 12 2 16 3 15 5 8 10 9 
# 11 4 12 9 7 16 5 2 8 10 13 3 1 14 15 6 
# 8 2 5 6 14 1 15 9 12 7 4 16 13 10 11 3 
# 14 1 3 15 6 4 13 10 11 9 2 5 8 16 12 7 
# 10 16 13 7 8 11 12 3 1 14 15 6 9 5 4 2
#
problem_34 = [[13, 9, 2, 0, 0, 0, 0, 0,16, 0, 0, 0, 4, 3, 0, 0],
              [ 4,12,15, 0, 0, 0, 0, 0, 9,13, 0, 2, 0, 6,14,11],
              [ 0,14, 0, 1, 0, 0, 0, 0,15, 0, 8,11,12, 0, 0,10],
              [16, 5, 6, 0, 0, 0, 0, 0,10, 3,12, 0, 0, 0, 0, 1],
              [ 0, 7,16, 5,10, 8, 0, 0, 0, 0, 6, 1, 0, 0, 0, 0],
              [ 2, 0, 0, 0,12, 0, 0, 0, 0,11, 7, 0, 0, 0, 0, 0],
              [ 0, 0,10,14, 0, 9, 6, 4, 0, 0,16, 0, 0, 0, 0, 0],
              [ 0,15, 9, 0, 5, 0, 7, 0, 4, 0, 0, 0, 0, 0, 0, 0],
              [ 0, 0, 0, 0, 0, 2, 9, 0, 0, 0, 0,10, 0,12, 0, 0],
              [ 0, 0, 0, 0, 0, 0, 0, 0, 6, 4, 5,13, 0, 1, 0, 0],
              [ 0, 0, 0, 0,13, 0, 0, 0, 0, 1, 0,12, 0,11, 7,15],
              [ 0, 0, 0, 0, 0,14, 0,12, 2,16, 0, 0, 0, 8,10, 9],
              [11, 0, 0, 9, 0,16, 5, 2, 0, 0, 0, 0, 0,14,15, 6],
              [ 0, 2, 5, 6, 0, 0,15, 0, 0, 0, 0, 0,13, 0,11, 0],
              [14, 1, 3, 0, 6, 0,13, 0, 0, 0, 0, 0, 0, 0, 0, 7],
              [10, 0, 0, 0, 8,11,12, 3, 0, 0, 0, 0, 9, 5, 4, 0]]


# 
# This problem is problem 89 from
# Gecode's sudoku.cpp
# http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
#
# Size : 25 x 25
#
# 11  23  13  10  19  16  6  2  24  7  5  9  1  20  17  15  8  18  25  3  4  12  21  22  14
# 15  16  4  22  18  11  8  21  20  10  25  2  14  13  24  7  12  19  23  9  17  5  6  1  3
# 21  1  5  20  25  3  18  15  9  22  11  16  8  4  12  17  14  13  6  24  7  23  19  10  2
# 3  8  12  9  24  19  17  14  23  4  7  21  6  22  10  16  11  1  2  5  15  18  20  13  25
# 17  14  7  6  2  1  5  13  12  25  3  18  19  23  15  4  20  22  10  21  11  16  9  24  8
# 22  19  23  21  13  6  2  3  17  24  4  7  12  1  9  11  15  25  16  8  18  14  5  20  10
# 25  18  2  24  8  22  4  19  16  21  14  11  5  10  13  23  17  6  20  1  9  3  12  15  7
# 6  10  17  3  16  5  12  7  8  9  15  20  2  25  18  22  19  14  24  13  1  11  23  21  4
# 1  11  14  12  9  20  15  10  25  13  6  8  23  16  21  18  4  5  3  7  19  17  22  2  24
# 5  20  15  4  7  14  1  11  18  23  17  19  3  24  22  9  2  21  12  10  6  8  13  25  16
# 20  24  10  13  15  23  11  17  19  3  21  1  16  7  2  12  5  9  4  25  8  22  14  18  6
# 4  5  16  14  12  25  10  18  6  2  23  13  15  8  19  1  24  3  17  22  20  21  7  9  11
# 18  22  21  11  3  8  16  24  4  12  9  17  25  14  5  20  10  15  7  6  2  13  1  19  23
# 19  7  6  2  1  9  13  5  22  15  20  24  4  18  11  8  21  16  14  23  25  10  3  12  17
# 9  17  25  8  23  7  14  20  21  1  12  10  22  6  3  2  13  11  19  18  24  15  16  4  5
# 10  13  19  16  11  18  24  6  3  17  1  5  20  12  7  25  9  2  21  15  23  4  8  14  22
# 12  25  8  15  21  10  19  23  14  11  2  4  13  17  16  3  1  7  22  20  5  9  24  6  18
# 14  4  18  5  22  15  20  9  2  16  19  23  21  3  8  10  6  24  13  17  12  7  25  11  1
# 7  2  24  1  20  12  21  25  13  8  18  22  11  9  6  14  23  4  5  16  10  19  17  3  15
# 23  3  9  17  6  4  7  22  1  5  10  14  24  15  25  19  18  12  8  11  21  20  2  16  13
# 13  12  20  19  10  17  3  16  11  6  22  15  7  5  1  21  25  23  18  2  14  24  4  8  9
# 24  9  11  18  14  13  22  8  10  19  16  25  17  21  23  6  7  20  1  4  3  2  15  5  12
# 16  6  22  25  5  2  23  4  15  18  8  12  9  19  20  24  3  17  11  14  13  1  10  7  21
# 2  21  3  23  4  24  9  1  7  20  13  6  10  11  14  5  16  8  15  12  22  25  18  17  19
# 8  15  1  7  17  21  25  12  5  14  24  3  18  2  4  13  22  10  9  19  16  6  11  23  20

problem_89 = [[11,23,13,10,19,16, 6, 2,24, 7, 5, 9, 1,20,17,15, 8,18,25, 3, 4,12,21,22,14],
              [15,16, 0,22, 0,11, 8, 0, 0, 0,25, 0,14, 0, 0, 0,12,19, 0, 0,17, 0, 0, 0, 0],
              [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,16, 0, 4, 0,17, 0,13, 0,24, 0,23,19,10, 2],
              [ 0, 0, 0, 0, 0,19, 0,14,23, 4, 0,21, 6,22,10, 0,11, 0, 2, 0, 0, 0, 0, 0, 0],
              [17,14, 0, 0, 2, 0, 0,13,12, 0, 0, 0, 0, 0,15, 4,20,22,10, 0,11, 0, 9,24, 8],
              [22, 0, 0, 0, 0, 6, 2, 0, 0, 0, 4, 7,12, 1, 9, 0, 0, 0, 0, 0, 0,14, 5, 0, 0],
              [ 0,18, 2, 0, 8,22, 0,19,16,21, 0, 0, 0,10,13,23, 0, 0,20, 0, 0, 3, 0,15, 7],
              [ 0, 0,17, 3, 0, 5, 0, 0, 8, 9, 0, 0, 0, 0,18, 0,19, 0, 0, 0, 0, 0,23,21, 0],
              [ 1,11, 0, 0, 9, 0,15,10,25, 0, 6, 0,23, 0, 0, 0, 0, 5, 3, 7, 0,17, 0, 0,24],
              [ 0, 0, 0, 0, 0, 0, 1, 0, 0,23, 0, 0, 0,24, 0, 0, 0,21,12, 0, 6, 8, 0,25,16],
              [20,24,10, 0,15,23,11,17, 0, 0, 0, 0, 0, 7, 0,12, 0, 0, 0, 0, 0,22, 0, 0, 6],
              [ 4, 5, 0,14,12,25, 0,18, 0, 0,23, 0,15, 0,19, 1, 0, 0, 0,22,20, 0, 7, 9, 0],
              [18, 0,21, 0, 0, 8, 0,24, 0, 0, 9, 0,25, 0, 0, 0,10, 0, 0, 0, 2, 0, 1,19, 0],
              [ 0, 0, 6, 2, 1, 0,13, 0,22, 0, 0, 0, 0, 0,11, 8,21,16, 0, 0,25, 0, 0,12,17],
              [ 0,17,25, 0,23, 7,14, 0,21, 1, 0, 0, 0, 0, 3, 0, 0,11, 0, 0,24, 0,16, 4, 5],
              [ 0, 0, 0, 0,11,18,24, 0, 0, 0, 0, 5, 0,12, 0,25, 0, 0, 0,15,23, 4, 8,14, 0],
              [ 0, 0, 0,15,21, 0, 0, 0, 0, 0, 2, 0,13,17, 0, 0, 1, 7, 0, 0, 5, 9,24, 0, 0],
              [ 0, 0,18, 0,22,15, 0, 0, 2,16, 0,23, 0, 0, 0,10, 6,24, 0,17,12, 0,25,11, 0],
              [ 7, 2, 0, 1, 0, 0,21, 0, 0, 0,18,22, 0, 9, 6,14, 0, 4, 5,16, 0, 0, 0, 0, 0],
              [ 0, 0, 9, 0, 0, 0, 7,22, 0, 0,10, 0,24, 0, 0, 0,18, 0, 0, 0,21, 0, 0, 0, 0],
              [ 0,12, 0,19,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,14, 0, 4, 8, 0],
              [24, 0,11,18, 0, 0, 0, 0, 0, 0, 0,25,17,21, 0, 6, 0, 0, 1, 0, 0, 0, 0, 5,12],
              [16, 6,22, 0, 0, 0,23, 4,15,18, 8, 0, 0, 0,20, 0, 0,17, 0,14, 0, 0, 0, 0, 0],
              [ 0,21, 0, 0, 4, 0, 9, 1, 7, 0, 0, 0, 0,11,14, 0,16, 8,15, 0,22, 0,18, 0, 0],
              [ 8,15, 0, 0, 0, 0, 0, 0, 5, 0,24, 3, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0, 0, 0,20]]



problems = {
    "world_hardest": {
       "problem": world_hardest,
       "size": 3
    },
    "another" : {
       "problem": another,
       "size" : 3
    },
    "problem_34" : {
       "problem" : problem_34,
       "size" : 4
    },
    "problem_89" : {
      "problem" : problem_89,
      "size": 5
    }
    }

prove_unicity = False
for p in problems:
    problem = problems[p]["problem"]
    size = problems[p]["size"]
    print(f"problem: {p} {size**2} x {size**2}")
    t0 = time.time()
    sudoku_ip(problem, prove_unicity)
    t1 = time.time()
    print("time:", t1-t0,"\n")

