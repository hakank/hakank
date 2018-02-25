#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Appointment scheduling in Z3
#
# From Stack Overflow
# "Appointment scheduling algorithm (N people with N free-busy slots, constraint-satisfaction)"
# http://stackoverflow.com/questions/11143439/appointment-scheduling-algorithm-n-people-with-n-free-busy-slots-constraint-sa
# ""
# Problem statement
# We have one employer that wants to interview N people, and therefore makes N 
# interview slots. Every person has a free-busy schedule for those slots. Give an algorithm 
# that schedules the N people into N slots if possible, and return a flag / error / etc if 
# it is impossible. What is the fastest possible runtime complexity?
#
# My approaches so far
#
# Naive: there are N! ways to schedule N people. Go through all of them, for each permutation, 
# check if it's feasible. O( n! )
#
# Backtracking:
#
#  Look for any interview time slots that can only have 1 person. Schedule the person, 
#  remove them from the list of candidates and remove the slot.
#  Look for any candidates that can only go into 1 slot. Schedule the person, remove them 
#  from the list of candidates and remove the slot.
#  Repeat 1 & 2 until there are no more combinations like that.
#  Pick a person, schedule them randomly into one of their available slots. Remember 
#  this operation.
#  Repeat 1, 2, 3 until we have a schedule or there is an unresolvable conflict. If we have a 
#  schedule, return it. If there's an unresolvable conflict, backtrack.
#
# This is O( n! ) worst case, I think - which isn't any better.
#
# There might be a D.P. solution as well - but I'm not seeing it yet.
#
# Other thoughts
#
# The problem can be represented in an NxN matrix, where the rows are "slots", columns are 
# "people", and the values are "1" for free and "0" for busy. Then, we're looking for a 
# row-column-swapped Identity Matrix within this matrix. Steps 1 & 2 are looking for a row or a 
# column with only one "1". (If the rank of the matrix is = N, I that means that there is a 
# solution. But the opposite does not hold) Another way to look at it is to treat the matrix 
# as an unweighed directed graph edge matrix. Then, the nodes each represent 1 candidate and 1 
# slot. We're then looking for a set of edges so that every node in the graph has one outgoing 
# edge and one incoming edge. Or, with 2x nodes, it would be a bipartite graph.
#
# Example of a matrix:
#
# 1 1 1 1
# 0 1 1 0
# 1 0 0 1
# 1 0 0 1
#
# I have a feeling that this might be NP-C.
# ""
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

#
# matrix based approach
#
def appointment_scheduling1(m):

    sol = Solver()
    
    n = len(m)

    # decision variables

    # the assignment of persons to a slot (appointment number 0..n)
    x = makeIntVectorMatrix(sol, "x", n,n, 0,1)

    # constraints

    for i in range(n):
        # ensure a free slot
        sol.add(Sum([m[i][j]*x[(i,j)] for j in range(n)]) == 1)

        # ensure one assignment per slot
        sol.add(Sum([x[(i,j)] for j in range(n)]) == 1)
        sol.add(Sum([x[(j,i)] for j in range(n)]) == 1)


    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        for i in range(n):
            for j in range(n):
                print mod[x[(i,j)]],
            print
        print
        sol.add(Or([x[(i,j)] != mod[x[(i,j)]] for i in range(n) for j in range(n)]))

    print "num_solutions:", num_solutions


# "set based" approach
def appointment_scheduling2(m):

    sol = Solver()
    
    n = len(m)

    # decision variables

    # the assignment of persons to a slot (appointment number 0..n)
    x = makeIntVector(sol, "x", n, 0,n-1)

    # constraints

    sol.add(Distinct(x))
    
    for i in range(n):
        # ensure a free solot
        member_of(sol, x[i], m[i])

    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        print [mod[x[i]] for i in range(n)]
        getDifferentSolution(sol,mod,x)

    print "num_solutions:", num_solutions



# rows are time slots
# columns are people
print "matrix approach"
m1 = [[1, 1, 1, 1],
      [0, 1, 1, 0],
      [1, 0, 0, 1],
      [1, 0, 0, 1]]

appointment_scheduling1(m1)
print
print "'set' approach"
# as "sets" of time slots
n = len(m1)
m2 = [ [j for j in range(len(m1)) if m1[i][j] == 1] for i in range(len(m1)) ] 
appointment_scheduling2(m2)
