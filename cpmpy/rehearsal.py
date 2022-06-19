"""
Scheduling a Rehearsal in Comet.

From Barbara M. Smith: 
'Constraint Programming in Practice: Scheduling a Rehearsal'
http://www.dcs.st-and.ac.uk/~apes/reports/apes-67-2003.pdf
'''
A concert is to consist of nine pieces of music of different durations 
each involving a different combination of the five members of the orchestra. 
Players can arrive at rehearsals immediately before the first piece in which 
they are involved and depart immediately after the last piece in which 
they are involved. The problem is to devise an order in which the pieces 
can be rehearsed so as to minimize the total time that players are waiting 
to play, i.e. the total time when players are present but not currently 
playing. In the table below, 1 means that the player is required for 
the corresponding piece, 0 otherwise. The duration (i.e. rehearsal time) 
is in some unspecified time units.

   Piece       1    2   3    4    5  6    7   8    9
   Player 1    1    1   0    1    0  1    1   0    1
   Player 2    1    1   0    1    1  1    0   1    0
   Player 3    1    1   0    0    0  0    1   1    0
   Player 4    1    0   0    0    1  1    0   0    1
   Player 5    0    0   1    0    1  1    1   1    0
   Duration    2    4   1    3    3  2    5   7    6

For example, if the nine pieces were rehearsed in numerical order as 
given above, then the total waiting time would be:
      Player 1: 1+3+7=11
      Player 2: 1+5=6
      Player 3: 1+3+3+2=9
      Player 4: 4+1+3+5+7=20
      Player 5: 3
giving a total of 49 units. The optimal sequence, as we shall see, 
is much better than this.

...

The minimum waiting time for the rehearsal problem is 17 time units, and 
an optimal sequence is 3, 8, 2, 7, 1, 6, 5, 4, 9.

'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math,random
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def rehearsal_model(rehearsal,duration,timeout=None,num_procs=1):

    # Convert to cpm_arrays since we
    # use element.
    rehearsal = cpm_array(rehearsal)
    duration = cpm_array(duration)
    
    num_players = len(rehearsal)    
    num_pieces = len(rehearsal[0])
    
    rehearsal_flat = flatten_lists(rehearsal)

    # decision variables
    rehearsal_order = intvar(0,num_pieces-1,shape=num_pieces,name="rehearsal_order")   
    waiting_time = intvar(0,sum(duration),shape=num_players,name="waiting_time")
    total_waiting_time = intvar(0,100,name="total_waiting_time")
    p_from = intvar(0,num_pieces-1,shape=num_players,name="p_from")
    p_to = intvar(0,num_pieces-1,shape=num_players,name="p_to")    

    # constraints
    model = Model([AllDifferent(rehearsal_order),
                    total_waiting_time == sum(waiting_time),
                   ],
        minimize=total_waiting_time)

    for p in range(num_players):
        # Fix the range from..to and trim those that starts with 0 
        # or ends with 0.
        # This means that we sort the rehearsals with many 0 at either ends.
        model += (p_from[p] < p_to[p])
                  
        for i in range(num_pieces):
            # skip rehearsal at start (don't come yet)
            model += ((i < p_from[p]).implies(Element(rehearsal_flat,p*num_pieces+rehearsal_order[i]) == 0))
    
            # skip rehearsal at end (go home after last rehearsal)
            model += ((i > p_to[p]).implies(Element(rehearsal_flat,p*num_pieces+rehearsal_order[i]) == 0))


        # and now: count the waiting time for from..to
        model += (waiting_time[p] ==
                  sum([  ((i >= p_from[p]) & (i <= p_to[p])) *
                         (duration[rehearsal_order[i]] * (Element(rehearsal,p*num_pieces+rehearsal_order[i]) == 0 ))
                         for i in range(num_pieces) ]))  
  

  
    # symmetry breaking
    model += (rehearsal_order[0] < rehearsal_order[num_pieces-1])

    def print_sol():
        print("rehearsal_order:\n", rehearsal_order.value())        
        print("waiting_time:", waiting_time.value())
        print("total_waiting_time:", total_waiting_time.value())
        print("p_from:", p_from.value())
        print("p_to  :", p_to.value())
        print("rehearsal schedule in order:")
        print(rehearsal[:,rehearsal_order.value()])
        print(flush=True)
        
    ss = CPM_ortools(model)
    if timeout != None:
        ss.ort_solver.parameters.max_time_in_seconds = timeout # seconds
    # ss.ort_solver.parameters.num_search_workers = num_procs # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.log_search_progress = True
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=1,display=print_sol)
    print("number of solutions:", num_solutions)    
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print("Status:", ss.status())    
    print()

rehearsal_problems = {
    #
    # This is the problem from Barbara M. Smith's Rehearsal paper cited above:
    #
    "smith1" : {
    "rehearsal" : [[1,1,0,1,0,1,1,0,1],
                  [1,1,0,1,1,1,0,1,0],
                  [1,1,0,0,0,0,1,1,0],
                  [1,0,0,0,1,1,0,0,1],
                  [0,0,1,0,1,1,1,1,0]],
    "duration" : [2, 4, 1, 3, 3, 2, 5, 7, 6]
    },
    #
    # This is the problem from the Choco v 2.1 example 
    # sample.scheduling.Rehearsal, the one defined in main() .
    #
    "choco" : {
    "rehearsal" : [[1,1,0,1,0],
               [0,1,1,0,1],
               [1,1,0,1,1]],
    "duration" : [4,6,3,5,7]
    },

    #
    # From Barbara M. Smith
    # "Constraint Programming in Practice: Scheduling a Rehearsal", page 9f.
    # This is the problem of Talent scheduling (shooting a movie).
    # In this model we don't care about the payment for the actors/players.
    #
    "smith2" : {
    "rehearsal" : [
    # Scene  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
            [1,1,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0], # Player 1
            [1,1,1,0,0,0,1,1,0,1,0,0,1,1,1,0,1,0,0,1], # Player 2  
            [0,1,1,0,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0], # Player 3  
            [0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0], # Player 4  
            [0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,1,1], # Player 5  
            [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0], # Player 6  
            [0,0,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0], # Player 7   
            [0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]  # Player 8
        ],
  "duration" : [2,1,1,1,1,3,1,1,1,2,1,1,2,1,2,1,1,2,1,1],
  
 }


}

timeout = 10
num_procs = 12
print("timeout:",timeout,"num_procs:",num_procs)
for p in rehearsal_problems:
    print(f"\nproblem {p}")
    rehearsal = rehearsal_problems[p]["rehearsal"]
    duration = rehearsal_problems[p]["duration"]
    rehearsal_model(rehearsal,duration,timeout,num_procs)

