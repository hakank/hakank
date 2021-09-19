"""
Global constraint contiguity using regular in cpmpy.

This is a decomposition of the global constraint global contiguity
implementing using regular constraint.

From Global Constraint Catalogue
http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
'''
Enforce all variables of the VARIABLES collection to be assigned to 0 or 1.
In addition, all variables assigned to value 1 appear contiguously.

Example:
(<0, 1, 1, 0>)

The global_contiguity constraint holds since the sequence 0 1 1 0 contains
no more than one group of contiguous 1.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

# Callback function for printing the solution
def print_solution(x):
    for a in x:
        print([val -1 for val in a.value()])

def contiguity_regular(n=7,num_sols=0):
    #
    # data
    #
    # the DFA (for regular)
    n_states = 3
    input_max = 2
    initial_state = 1  # 0 is for the failing state

    # all states are accepting states
    accepting_states = [1, 2, 3]

    # The regular expression 0*1*0*
    transition_fn = [
        [1, 2],  # state 1 (start): input 0 -> state 1, input 1 -> state 2 i.e. 0*
        [3, 2],  # state 2: 1*
        [3, 0],  # state 3: 0*
        ]

    #
    # declare variables
    #

    # We use domain 1..2 and then subtract 1 in the solution
    reg_input = intvar(1,2,shape=n,name="reg_input")

    model = Model() 
    #
    # constraints
    #
    model += [regular(reg_input, n_states, input_max, transition_fn, initial_state,
                      accepting_states)]


    ortools_wrapper(model, [reg_input],print_solution,num_sols)



contiguity_regular(7)
