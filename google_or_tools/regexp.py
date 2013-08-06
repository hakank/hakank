# -*- coding: utf-8 -*-
# Copyright 2010 Hakan Kjellerstrand hakank@bonetmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
#
#     http://www.apache.org/licenses/LICENSE-2.0 
#
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 

"""

  Simple regular expression in Google CP Solver.

  My last name (Kjellerstrand) is quite often misspelled
  in ways that this regular expression shows:
    k(je|ä)ll(er|ar)?(st|b)r?an?d

  This model generates all the words that can be construed
  by this regular expression.

  Compare with the following model:
  * Gecode: http://www.hakank.org/gecode/all_regexp.cpp

  This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  Also see my other Google CP Solver models: http://www.hakank.org/google_or_tools/
  
"""

from constraint_solver import pywrapcp

#
# Global constraint regular
#
# This is a translation of MiniZinc's regular constraint (defined in 
# lib/zinc/globals.mzn), via the Comet code refered above.
# All comments are from the MiniZinc code.
# '''
# The sequence of values in array 'x' (which must all be in the range 1..S)
# is accepted by the DFA of 'Q' states with input 1..S and transition
# function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
# (which must be in 1..Q) and accepting states 'F' (which all must be in
# 1..Q).  We reserve state 0 to be an always failing state.
# '''
#
# x : IntVar array
# Q : number of states
# S : input_max
# d : transition matrix
# q0: initial state
# F : accepting states
def regular(x, Q, S, d, q0, F):

    solver = x[0].solver()
    
    assert Q > 0, 'regular: "Q" must be greater than zero'
    assert S > 0, 'regular: "S" must be greater than zero'

    # d2 is the same as d, except we add one extra transition for
    # each possible input;  each extra transition is from state zero
    # to state zero.  This allows us to continue even if we hit a
    # non-accepted input.
    
    # Comet: int d2[0..Q, 1..S]
    d2 = []
    for i in range(Q+1):
        row = []
        for j in range(S):
            if i == 0:
                row.append(0)
            else:
               row.append(d[i-1][j])
        d2.append(row)

    d2_flatten = [d2[i][j] for i in range(Q+1) for j in range(S)]

    # If x has index set m..n, then a[m-1] holds the initial state
    # (q0), and a[i+1] holds the state we're in after processing
    # x[i].  If a[n] is in F, then we succeed (ie. accept the
    # string).
    x_range = range(0,len(x))
    m = 0
    n = len(x)
    
    a = [solver.IntVar(0, Q+1, 'a[%i]' % i) for i in range(m, n+1)] 
    
    # Check that the final state is in F
    solver.Add(solver.MemberCt(a[-1], F))
    # First state is q0
    solver.Add(a[m] == q0)    
    for i in x_range:
        solver.Add(x[i] >= 1)
        solver.Add(x[i] <= S)
        
        # Determine a[i+1]: a[i+1] == d2[a[i], x[i]]
        solver.Add(a[i+1] == solver.Element(d2_flatten, ((a[i])*S)+(x[i]-1)))

       


def main(n, res):
    
    # Create the solver.
    solver = pywrapcp.Solver('Regular expression')

    #
    # data
    #
    # the DFA (for regular)
    n_states = 11
    input_max = 12
    initial_state = 1 # 0 is for the failing state
    accepting_states = [12]

    # The DFA
    transition_fn =  [
       # 1 2 3 4 5 6 7 8 9 0 1 2     # 
        [0,2,3,0,0,0,0,0,0,0,0,0],   #  1 k 
        [0,0,0,4,0,0,0,0,0,0,0,0],   #  2 je
        [0,0,0,4,0,0,0,0,0,0,0,0],   #  3 ä
        [0,0,0,0,5,6,7,8,0,0,0,0],   #  4 ll
        [0,0,0,0,0,0,7,8,0,0,0,0],   #  5 er
        [0,0,0,0,0,0,7,8,0,0,0,0],   #  6 ar
        [0,0,0,0,0,0,0,0,9,10,0,0],  #  7 st 
        [0,0,0,0,0,0,0,0,9,10,0,0],  #  8 b
        [0,0,0,0,0,0,0,0,0,10,0,0],  #  9 r
        [0,0,0,0,0,0,0,0,0,0,11,12], # 10 a
        [0,0,0,0,0,0,0,0,0,0,0,12],  # 11 n
                                     # 12 d 
        ]

    s = ['k','je','ä','ll','er','ar','st','b','r','a','n','d']
    print 'n:', n

    #
    # declare variables
    #

    x = [solver.IntVar(1, 12, 'x[%i]'% i) for i in range(n)]


    #
    # constraints
    #
    regular(x, n_states, input_max, transition_fn,
            initial_state, accepting_states)
  
    #
    # solution and search
    #   
    db = solver.Phase(x,
                      solver.CHOOSE_FIRST_UNBOUND,                     
                      solver.ASSIGN_MIN_VALUE)

    solver.NewSearch(db)
    
    num_solutions = 0
    while solver.NextSolution():
        num_solutions += 1
        # Note: 1 is the start state which is not included in the
        #       state array (x)
        x_val =  [1] + [x[i].Value() for i in range(n)]
        # print 'x:', x_val
        ss = ''.join([str(s[i-1]) for i in x_val])
        res.append(ss)
        # print
        
    solver.EndSearch()

    print 'num_solutions:', num_solutions
    print 'failures:', solver.failures()
    print 'branches:', solver.branches()
    print 'wall_time:', solver.wall_time(), 'ms'
    print


if __name__ == '__main__':
    res = []
    for n in range(4,9+1):
        main(n, res)
    print 'The following %i words where generated:' % len(res)
    # res.sort(key=len)
    for r in res:
        print r
