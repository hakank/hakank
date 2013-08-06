#!/usr/bin/python
"""
Hidato puzzle in Numberjack.
  
http://www.shockwave.com/gamelanding/hidato.jsp
http://www.hidato.com/
'''
Puzzles start semi-filled with numbered tiles.
The first and last numbers are circled.
Connect the numbers together to win. Consecutive
number must touch horizontally, vertically, or
diagonally.
'''

Compare with the following models: 
* MiniZinc: http://www.hakank.org/minizinc/hidato.mzn
* Gecode  : http://www.hakank.org/gecode/hidato.cpp
* Comet   : http://www.hakank.org/comet/hidato.co


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

# from NumberjackSolver import Solver
from Mistral import Solver
# from SCIP import Solver



#
# Solve the Hidato problem
#
def hidato(libs):

    #
    # Simple problem
    #
#     r = 3
#     c = r
#     puzzle = [ 
#         [6,0,9],
#         [0,2,8],
#         [1,0,0]
#         ]


#     r = 7
#     c = 7
#     puzzle =  [ 
#         [0,44,41, 0, 0, 0, 0],
#         [0,43, 0,28,29, 0, 0],
#         [0, 1, 0, 0, 0,33, 0],
#         [0, 2,25, 4,34, 0,36],
#         [49,16, 0,23, 0, 0, 0],
#         [0,19, 0, 0,12, 7, 0],
#         [0, 0, 0,14, 0, 0, 0] 
#         ]
    

    # Problems from the book:
    # Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"
    #
    # Problem 1 (Practice)
    r = 5
    c = r
    puzzle = [
       [ 0, 0,20, 0, 0],
       [ 0, 0, 0,16,18],
       [22, 0,15, 0, 0],
       [23, 0, 1,14,11],
       [ 0,25, 0, 0,12],
       ]


#     # problem 2 (Practice)
#     r = 5
#     c = r
#     puzzle= [
#         [0, 0, 0, 0,14],
#         [0,18,12, 0, 0],
#         [0, 0,17, 4, 5],
#         [0, 0, 7, 0, 0],
#         [9, 8,25, 1, 0],
#         ];

    # problem 3 (Beginner)
#     r = 6
#     c = r
#     puzzle =  [
#         [ 0, 26,0, 0, 0,18],
#         [ 0, 0,27, 0, 0,19],
#         [31,23, 0, 0,14, 0],
#         [ 0,33, 8, 0,15, 1],
#         [ 0, 0, 0, 5, 0, 0],
#         [35,36, 0,10, 0, 0]
#         ];


    # Problem 15 (Intermediate)
    # Note: This takes very long time to solve...
#     r = 8
#     c = r
#     puzzle = [
#          [64, 0, 0, 0, 0, 0, 0, 0],
#          [ 1,63, 0,59,15,57,53, 0],
#          [ 0, 4, 0,14, 0, 0, 0, 0],
#          [ 3, 0,11, 0,20,19, 0,50],
#          [ 0, 0, 0, 0,22, 0,48,40],
#          [ 9, 0, 0,32,23, 0, 0,41],
#          [27, 0, 0, 0,36, 0,46, 0],
#          [28,30, 0,35, 0, 0, 0, 0]
#          ]


    print_game(puzzle, r,c)

    #
    # Decision variables
    # Note: Matrix is defined with cols,rows,...
    #
    x = Matrix(c,r,1,r*c)

    model = Model(
                 AllDiff(x.flat)
                 )

    #
    # Fill in the clues
    #
    for i in range(r):
        for j in range(c):           
            if puzzle[i][j] > 0:
                model.add(x[i,j] == puzzle[i][j])

    # From the numbers k = 1 to r*c-1, find this position,
    # and then the position of k+1
    for k in range(1,r*c):
        i = Variable(0,r)
        j = Variable(0,c)
        a = Variable(-1,1)
        b = Variable(-1,1)

        # 1) First: fix "this" k
        # 2) and then find the position of the next value (k+1)
        model.add([
               k == x[i,j],
               k + 1 == x[i+a,j+b]
               ])

        model.add([
               i+a >= 0,
               j+b >= 0,
               i+a < r,
               j+b < c,
               ((a != 0) | (b != 0))
               ]) 
  
    for library in libs:
        solver = model.load(library)
        
        # Using search heuristics makes is much faster:
        # Here is statistics for problem1 (see above), and
        # requiring all solutions:
        # With variable heuristics DomainOverWDegree there are
        # 91/254 failures instead of 5769/35091 (no explicit heuristic)
        # where first/all is the failures for the _first_ solution
        # and _all_ is for searching the complete tree.
        # Some other heuristics:
        # solver.setHeuristic("DomainOverWDegree") # 91/254
        # solver.setHeuristic("Impact") # Also quite fast: 103/927 failures
        # solver.setHeuristic("MaxDegree") # 90/292 failures
        # The following is the fastest, i.e. combined with value heuristics
        # "AntiLex": 68/142 failures
        solver.setHeuristic("DomainOverWDegree", "AntiLex")
        print ''
        if solver.solve():
            solver.printStatistics()
            print_board(x, r, c)
            num_solutions = 1
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime(), " Failures: ", solver.getFailures( )           
            while library == 'Mistral' and solver.getNextSolution():
                print_board(x, r, c)
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime(), " Failures: ", solver.getFailures()
                num_solutions += 1
            print "number of solutions:", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime(), " Failures: ", solver.getFailures()
        else:
            print "No solution"
        print ''

#
# Read a problem instance from a file
#
def read_problem(file):
    f = open(file, 'r')
    rows = int(f.readline())
    cols = int(f.readline())
    game = []
    for i in range(rows):
        x = f.readline()
        row = [0]*cols
        for j in range(cols):
            if x[j] == ".":
                tmp = -1
            else:
                tmp = int(x[j])
            row[j] = tmp
        game.append(row)
    return [game, rows, cols]


#
# Print the mines
#
def print_board(x, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print "% 2s" % x[i,j],
        print ''

def print_game(game, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print "% 2s" % game[i][j],
        print ''
            

if len(sys.argv) > 1:
    file = sys.argv[1]
    print "Problem instance from", file
    [game, rows, cols] = read_problem(file)
    print_game(game, rows, cols)
    hidato(('NumberjackSolver','Mistral'), game, rows, cols)
else:    
    # hidato(['NumberjackSolver'])
    hidato(['Mistral'])
    #hidato(['SCIP'])
