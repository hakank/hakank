"""
Test of decomposition of regular constraint_table in cpmpy.

This is contiguity_regular.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def regular_table_test():
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

    n = 7

    #
    # declare variables
    #

    # We use 1..2 and subtract 1 in the solution
    # reg_input = [solver.IntVar(1, 2, 'x[%i]' % i) for i in range(n)]
    reg_input = intvar(1,2,shape=n,name="reg_input")

    model = Model() 
    #
    # constraints
    #
    model += [regular_table(reg_input, n_states, input_max, transition_fn, initial_state,
                      accepting_states)]


    def print_sol():
        print("reg_input:", reg_input.value() - 1)
    
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)    
    print('num_solutions:', num_solutions)



regular_table_test()
