#!/usr/bin/python
"""
Quasigroup completion in Numberjack.

See Carla P. Gomes and David Shmoys:
"Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"

See also
Ivars Peterson "Completing Latin Squares"
http://www.maa.org/mathland/mathtrek_5_8_00.html
'''
Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
a four-by-four array so that no column or row contains the same two numbers. 
The result is known as a Latin square.
...
The so-called quasigroup completion problem concerns a table that is correctly 
but only partially filled in. The question is whether the remaining blanks in 
the table can be filled in to obtain a complete Latin square (or a proper 
quasigroup multiplication table).
'''

Compare with the following models: 
* Choco: http://www.hakank.org/choco/QuasigroupCompletion.java
* Comet: http://www.hakank.org/comet/quasigroup_completion.co
* ECLiPSE: http://www.hakank.org/eclipse/quasigroup_completion.ecl
* Gecode: http://www.hakank.org/gecode/quasigroup_completion.cpp
* Gecode/R: http://www.hakank.org/gecode_r/quasigroup_completion.rb
* JaCoP: http://www.hakank.org/JaCoP/QuasigroupCompletion.java
* MiniZinc: http://www.hakank.org/minizinc/quasigroup_completion.mzn
* Tailor/Essence': http://www.hakank.org/tailor/quasigroup_completion.eprime


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

from Mistral import Solver
# from SCIP import Solver

default_n = 5
X = 0

# default problem
# (This is the same as quasigroup1.txt)
default_puzzle = [
    [1, X, X, X, 4],
    [X, 5, X, X, X],
    [4, X, X, 2, X],
    [X, 4, X, X, X],
    [X, X, 5, X, 1]
    ]


def quasigroup_completion(libs, puzzle="", n=0):

    if puzzle == "":
        puzzle = default_puzzle
        n = default_n

    print_game(puzzle, n,n)

    #
    # Decision variables
    # Note: Matrix is defined with cols,rows,...
    #
    x = Matrix(n,n,1,n)

    model = Model()

    #
    # set the clues
    #
    for i in range(0,n):
        for j in range(0,n):
            if puzzle[i][j] > X:
                model.add(x[i,j] == puzzle[i][j])

    #
    # rows and columns must be different
    #
    model.add(
        [AllDiff(row) for row in x.row],
        [AllDiff(col) for col in x.col]
        )

 
    for library in libs:
        solver = model.load(library)
        
        solver.setHeuristic("DomainOverWDegree", "AntiLex")
        print ''
        if solver.solve():
            solver.printStatistics()
            print_board(x, n, n)
            num_solutions = 1
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            while library == 'Mistral' and solver.getNextSolution():
                print_board(x, n, n)
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime(), " Failures: ", solver.getFailures()
                print ''
                num_solutions += 1
            print "number of solutions:", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''

#
# Read a problem instance from a file
#
def read_problem(file):
    f = open(file, 'r')
    n = int(f.readline())
    game = []
    for i in range(n):
        x = f.readline()
        row_x = (x.rstrip()).split(" ")
        row = [0]*n
        for j in range(n):
            if row_x[j] == ".":
                tmp = 0
            else:
                tmp = int(row_x[j])
            row[j] = tmp
        game.append(row)
    return [game, n]


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
    [game, n] = read_problem(file)
    # quasigroup_completion(('Mistral','NumberjackSolver'), game, n)
    quasigroup_completion(['Mistral'], game, n)    
else:    
    # quasigroup_completion(['Mistral','SCIP'])
    quasigroup_completion(['Mistral'])    
