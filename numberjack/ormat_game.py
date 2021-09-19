#!/usr/bin/python
"""
Some experiment for the Ormat game in Numberjack.


This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *


# Generate all the overlays for a specific size (n)
def get_overlays(libs, n=3, debug=0):

    x = Matrix(n, n, 0, 1,'x')
    model = Model (
        [ Sum(row) == 1 for row in x.row],
        [ Sum(col) == 1 for col in x.col]        
        )

    #print model

    overlays = []
    for library in libs:
        solver = model.load(library) # Load up model into solver
        if solver.solve():
            num_solutions = 1
            if debug:
                print 'x (', num_solutions-1, '):\n',x
            overlays.append([[x[i,j].get_value() for j in range(n)] for i in range(n) ])
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                if debug:
                    print 'x (', num_solutions-1, '):\n',x                    
                overlays.append([[x[i,j].get_value() for i in range(n)] for j in range(n) ])                
            #print 'Number of solutions: ', num_solutions
            #print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            #print 'getPropags:', solver.getPropags()
            #print 'getBacktracks:', solver.getBacktracks()
            #print 'getFailures:', solver.getFailures()
        else:
            print 'No solution for getting the overlay'
        #print '\n\n'
    return overlays


# Generate a (random) problem.
# Not working as I want....
def generate_problem(libs, n = 3):
    x = Matrix(n, n, 0, 1,'x')

    model = Model (
        #Sum(x.flat) > 3
        )

    for library in libs:
        solver = model.load(library) # Load up model into solver
        solver.setHeuristic("Random","RandomMinMax",10)
        if solver.solve():
            print 'x:\n',x
            #print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            #print 'getPropags:', solver.getPropags()
            #print 'getBacktracks:', solver.getBacktracks()
            #print 'getFailures:', solver.getFailures()
        else:
            print 'No solution'
        print '\n\n'
    return x          


# Generate all the problems of size n
def all_problems(libs, n = 3, debug = 0):
    x = Matrix(n, n, 0, 1,'x')

    model = Model (
        # Sum(x.flat) >= n,
        
        [ Sum(row) >= 1 for row in x.row],
        [ Sum(col) >= 1 for col in x.col], 
        )

    problems = []
    for library in libs:
        solver = model.load(library)
        if solver.solve():
            num_solutions = 1
            if debug:
                print 'x1:\n',x
            problems.append([[x[i,j].get_value() for j in range(n)] for i in range(n) ])
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                if debug:
                    print "x2:",x
                problems.append([[x[i,j].get_value() for i in range(n)] for j in range(n) ])                       
        else:
            print 'No solution'
        print '\n\n'
    return problems
    

# print a solution
def print_solution(x, overlays):
    f = len(x)
    n = len(overlays[0])
    print "f:",f, " n: ", n
    for o in range(f):
        if x[o].get_value() == 1:
            print "overlay", o
            for i in range(n):
                for j in range(n):
                    print overlays[o][i][j], " ",
                print ''
            print ''


# print a problem
def print_problem(problem, n):
    print "Problem:"
    for i in range(n):
        for j in range(n):
            print problem[i][j], " ",
        print ''
    print ''
            

#
# This solves a problem instance
#
def ormat_game(libs, problem, overlays, n, debug=0):

    f = len(overlays)
    x = VarArray(f, 0, 1,'x');
    num_overlays = Variable(0,f,'num_overlays')

    # Count the number of occurrences for each cell for
    # the choosen overlays.
    # Mainly for debugging purposes, but it also makes the
    # modeling easier.
    y = Matrix(n,n,0,f,'y')
    
    model = Model (
        # sanity clauses
        [ Sum(row) >= 1 for row in y.row],
        [ Sum(col) >= 1 for col in y.col], 
        
        Minimize(num_overlays),
        num_overlays == Sum(x),
        )

    

    for i in range(n):
        for j in range(n):           
            model.add(y[i][j] == Sum([(x[o])*(overlays[o][i][j]) for o in range(f)]))

            # model.add(y[i][j] >= problem[i][j])
            
            if problem[i][j] == 1:
                ## This don't work: list indices must be integers, not tuple
                #o = Variable(0,f-1,'o')
                #model.add(x[o]*overlays[o,i,j] == 1)
                #
                # Alternative: sum of the overlays >= 1
                # This works.
                # model.add(Sum([x[o]*overlays[o][i][j] for o in range(f)]) >= 1)
                model.add(y[i][j] >= 1)

            if problem[i][j] == 0:
                # This is easier, given that we do all hard work above
                model.add(y[i][j] == 0)                    
                # Why don't these two guys work? Or rather: why don't they always work?
                # model.add(Sum([(x[o])*(overlays[o][i][j]) for o in range(f)]) == 0)
                #model.add([(x[o] == 0) | (x[o] == 1 & overlays[o][i][j] == 0) for o in range(f)])


    if debug:
        print model

    for library in libs:
        solver = model.load(library)
        if debug:
            print "x before solve:", x
            print "y before solve:\n", y
        num_solutions = 0
        if solver.solve():
            print "Solution:"
            print 'x:\n',x
            print 'y:\n',y
            print 'num_overlays:', num_overlays
            print_solution(x, overlays)
            num_solutions += 1
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                print 'x:\n',x
                print 'y:\n',y                
                print 'num_overlays:', num_overlays
                print_solution(x, overlays)

            print 'Number of solutions: ', num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print 'getPropags:', solver.getPropags()
            print 'getBacktracks:', solver.getBacktracks()
            print 'getFailures:', solver.getFailures()
        else:
            print 'No solution'
        print '\n\n'
        return num_solutions



# problem 1
# n = 3
# problem = [
#     [1,0,0],
#     [0,1,1],
#     [0,1,1]
#     ]

# # Problem grid 2
# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [1,1,1]
#     ]


# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [0,1,1]
#     ]


# Note: Before solve y matrix has y2.2 in {} which is very bad, and
#       is the reason (or a symptom) that this problem shows no solution.
#
# x before solve: [x0 in {0,1}, x1 in {1}, x2 in {0,1}, x3 in {1}, x4 in {1}, x5 in {0,1}]
# y before solve:
# [[y0.0 in {0..2}, y0.1 in {0,1}, y0.2 in {0..3}],
# [y1.0 in {0..3}, y1.1 in {0..2}, y1.2 in {0,1}],
# [y2.0 in {0,1}, y2.1 in {0..3}, y2.2 in {}]]
#
# Strange: another run has not this empty domain for y2.2...
#
# # Problem grid 3
# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [1,1,0]
#     ]


# This rotation of the above works
# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [0,1,1]
#     ]



# This is a _bad_ problem since all rows
# and colutions must have at least one cell=1
# n = 3
# problem = [
#     [0,0,0],
#     [0,1,1],
#     [0,1,1]
#     ]


# # Problem grid 4 (n = 4)
# n = 4
# problem = [
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,0,0]
#     ]


# variant
# n = 4
# problem = [
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,1,0]
#     ]

# variant
n = 4
problem = [
    [1,1,1,1],
     [1,1,1,1],
     [1,1,1,1],
     [1,1,1,1]
     ]



# # Problem grid 5 (n = 5)
# # This is under the section "Out of bounds"
# n = 5
# problem = [
#     [1,1,1,1,1],
#     [1,1,1,1,1],
#     [1,1,1,1,1],
#     [1,1,1,1,1],
#     [1,1,0,0,0]
#     ]

# # Problem grid 6 (n = 6)
# n = 6
# # This is under the section "Out of bounds"%
# problem = [
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,0,0,0,0]
#     ]



debug = 0
print_problem(problem, n)
overlays = get_overlays(['Mistral'], n, debug)
# print "overlays:", overlays

ormat_game(['Mistral'], problem, overlays, n, debug)
# ormat_game(['SCIP'], problem, overlays, n, debug) # seg faults

## Test all problems of a certain size
# n=3
# debug = 0
# num_problems = 0
# num_solved = 0
# overlays = get_overlays(['Mistral'], n, debug)
# problems = all_problems(['Mistral'], n, debug)
# print "num_problems:", len(problems)
# for p in problems:
#     print_problem(p,n)
#     num_problems += 1
#     num_sol = ormat_game(['Mistral'], p, overlays, n, debug)
#     num_solved += num_sol
#     # print "num_sol:",num_sol

# print "num_problems: ", num_problems
# print "num_solved:", num_solved
