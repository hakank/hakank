#!/usr/bin/python
"""
Minesweeper problem in Numberjack.

From gecode/examples/minesweeper.cc:
'''
A specification is a square matrix of characters. Alphanumeric 
characters represent the number of mines adjacent to that field. 
Dots represent fields with an unknown number of mines adjacent to 
it (or an actual mine).
'''

E.g.
     "..2.3."
     "2....."
     "..24.3"
     "1.34.."
     ".....3"
     ".3.3.."

Also see 
 
* http://www.janko.at/Raetsel/Minesweeper/index.htm

* http://en.wikipedia.org/wiki/Minesweeper_(computer_game)

* Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/

* Richard Kaye's Minesweeper Pages
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
  * Richard Kaye: "Some Minesweeper Configurations"
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf


Compare with my other Minesweeper models:
* MiniZinc: http://www.hakank.org/minizinc/minesweeper.mzn
* Choco   : http://www.hakank.org/choco/MineSweeper.java
* JaCoP   : http://www.hakank.org/JaCoP/MineSweeper.java
* Gecode/R: http://www.hakank.org/gecode_r/minesweeper.rb

This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

#from Mistral import Solver
#from SCIP import Solver

#
# Some notes:
# 1)
#   For the problem instance minesweeper6.txt
#   Mistral gives the correct solution, but when
#   asking for another solution, this error is thrown:
#    c Error: Solution does not verify
#    c       (b7 + b9 + b8) <= 0
#    c       b7 = 0
#    c       b9 = 0
#    c       b8 = 1
#    s NOT SUPPORTED
# 
#
#   Same with minesweeper9.txt
# 2)
#   For minesweeper_kaye_splitter.txt
#   not all cells in "mines" has been
#   assigned to a single value.
#   This model has a lot of solutions (131072).
#
#    


#
# Default problem from "Some Minesweeper Configurations",page 3
# (same as problem instance minesweeper_config3.txt)
# It has 4 solutions
# 
X = -1
default_game = [
            [2,3,X,2,2,X,2,1],
            [X,X,4,X,X,4,X,2],
            [X,X,X,X,X,X,4,X],
            [X,5,X,6,X,X,X,2],
            [2,X,X,X,5,5,X,2],
            [1,3,4,X,X,X,4,X],
            [0,1,X,4,X,X,X,3],
            [0,1,2,X,2,3,X,2]
            ]
default_r = 8
default_c = 8

#
# Solve the Minesweeper problem
#
def minesweeper(libs, game="", r="", c=""):

    # Set default problem
    if game == "":
        game = default_game
        r = default_r
        c = default_c
    else:
        print "rows:", r, " cols:", c

    #
    # Decision variables
    # Note: Matrix is defined with cols,rows,...
    #
    mines = Matrix(c,r,0,1)

    S = [-1,0,1]  # for the neighbors of this cell

    model = Model()
    
    for i in range(r):
        for j in range(c):
            if game[i][j] >= 0:
                model.add(mines[i,j] == 0)
                # this cell is the sum of all the surrounding cells
                model.add(
                    game[i][j] == Sum([mines[i+a,j+b]
                                       for a in S for b in S
                                       if i+a>=0 and
                                          j+b>=0 and
                                          i+a<r  and
                                          j+b<c
                                       ])
                    )
            if game[i][j] > X:
                # This cell cannot be a mine
                model.add(mines[i,j] == 0)

    # print model

  
    for library in libs:
        solver = model.load(library)
        # print solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print_mines(mines, r, c)            
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            while library == 'Mistral' and solver.getNextSolution():
                print_mines(mines, r, c)
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
                
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
def print_mines(mines, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print mines[i,j],
        print ''

def print_game(game, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print game[i][j],
        print ''
            

# file = "minesweeper1.txt"

if len(sys.argv) > 1:
    file = sys.argv[1]
    print "Problem instance from", file
    [game, rows, cols] = read_problem(file)
    print_game(game, rows, cols)
    # minesweeper(('SCIP','Mistral'), game, rows, cols)
    minesweeper(['Mistral'], game, rows, cols)    
else:    
    # minesweeper(['NumberjackSolver', 'Mistral'])
    minesweeper(['Mistral'])
    # minesweeper(['SCIP'])
