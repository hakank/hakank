"""
Appointment scheduling in cpmpy.

From Stack Overflow 
Appointment scheduling algorithm (N people with N free-busy slots, constraint-satisfaction)
http://stackoverflow.com/questions/11143439/appointment-scheduling-algorithm-n-people-with-n-free-busy-slots-constraint-sa
'''
Problem statement
We have one employer that wants to interview N people, and therefore makes N 
interview slots. Every person has a free-busy schedule for those slots. Give an 
algorithm that schedules the N people into N slots if possible, and return a 
flag / error / etc if it is impossible. What is the fastest possible runtime complexity?
My approaches so far
Naive: there are N! ways to schedule N people. Go through all of them, for each 
permutation, check if it's feasible. O( n! )
Backtracking:
1. Look for any interview time slots that can only have 1 person. Schedule the person, 
    remove them from the list of candidates and remove the slot.
2. Look for any candidates that can only go into 1 slot. Schedule the person, remove 
    them from the list of candidates and remove the slot.
3. Repeat 1 & 2 until there are no more combinations like that.
4. Pick a person, schedule them randomly into one of their available slots. Remember 
    this operation.
5. Repeat 1, 2, 3 until we have a schedule or there is an unresolvable conflict. If we 
    have a schedule, return it. If there's an unresolvable conflict, backtrack.
This is O( n! ) worst case, I think - which isn't any better.
There might be a D.P. solution as well - but I'm not seeing it yet.
Other thoughts
The problem can be represented in an NxN matrix, where the rows are "slots", columns 
are "people", and the values are "1" for free and "0" for busy. Then, we're looking for 
a row-column-swapped Identity Matrix within this matrix. Steps 1 & 2 are looking for 
a row or a column with only one "1". (If the rank of the matrix is = N, I that means that 
there is a solution. But the opposite does not hold) Another way to look at it is to 
treat the matrix as an unweighed directed graph edge matrix. Then, the nodes each 
represent 1 candidate and 1 slot. We're then looking for a set of edges so that every 
node in the graph has one outgoing edge and one incoming edge. Or, with 2x nodes, it would 
be a bipartite graph.

Example of a matrix:
    1 1 1 1
    0 1 1 0
    1 0 0 1
    1 0 0 1
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,random
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *




#
# matrix based approach
#
def appointment_scheduling1(m,num_sols=0):

    model = Model()
    
    n = len(m)

    # decision variables

    # the assignment of persons to a slot (appointment number 0..n)
    x = boolvar(shape=(n,n),name="x")

    # constraints

    for i in range(n):
        # ensure a free slot
        model += (sum([m[i][j]*x[(i,j)] for j in range(n)]) == 1)

        # ensure one assignment per slot
        model += (sum([x[(i,j)] for j in range(n)]) == 1)
        model += (sum([x[(j,i)] for j in range(n)]) == 1)

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(solution_limit=num_sols,display=x)
    print("num_solutions:", num_solutions)  


# "set based" approach
def appointment_scheduling2(m,num_sols=0):

    model = Model()
    
    n = len(m)

    # decision variables

    # the assignment of persons to a slot (appointment number 0..n)
    x = intvar(0,n-1,shape=n,name="x")

    # constraints

    model += (AllDifferent(x))
    
    for i in range(n):
        # ensure a free slot
        model += (member_of(m[i],x[i]))

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(solution_limit=num_sols,display=x)
    print("num_solutions:", num_solutions)  

def convert_to_sets(m):
  """
  Convert a matrix representation to sets.
  """
  return [ [j for j in range(len(m)) if m[i][j] == 1] for i in range(len(m)) ] 

def random_instance(n):
  """
  Generate a random matrix instance.
  """
  return [ [random.randint(0,1) for i in range(n)] for j in range(n)] 


#
# rows are time slots
# columns are people
#
m1 = [[1, 1, 1, 1],
      [0, 1, 1, 0],
      [1, 0, 0, 1],
      [1, 0, 0, 1]]

print("matrix approach")
appointment_scheduling1(m1)
print()

print("'set' approach")
appointment_scheduling2(convert_to_sets(m1))

print("\nrandom instance (show 2 solutions):")
mrand = random_instance(30)
appointment_scheduling1(mrand,2)
appointment_scheduling2(convert_to_sets(mrand),2)

