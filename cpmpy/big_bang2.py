"""
Big Bang problem in cpmpy.

This a port of a OR-tools CP-Solver model ported from
a MiniZinc model based on a Comet model by Thore Graepel
(which was based on a Comet model of mine):
'''
Nontransitive dice a la The Big Bang Theory in Comet 
Thore Graepel (thoregraepel@googlemail.com)

The idea is to create a set of five dice such that the dominance relationships
between the dice is isomorphic to the corresponding relationships in the game
Rock-Paper-Scissors extended by the choices Lizard and Spock.    
(originally from http://www.samkass.com/theories/RPSSL.html).

Each choice beats two other choices and is beaten by two other choices. 
In particular, we have the following ten 'beats' relationships    

1:  Rock(1) crushes Scissors(3)
2:  Rock(1) crushes Lizard(4)
3:  Paper(2) covers Rock(1)
4:  Paper(2) disproves Spock(5)
5:  Scissors(3) cuts Paper(2)
6:  Scissors(3) decapitate Lizard(4)
7:  Lizard(4) eats Paper(2)
8:  Lizard(4) poisons Spock(5)
9:  Spock(5) vaporizes Rock(1)
10: Spock(5) smashes Scissors(3)

Based on a Comet model for simple nontransitive dice 
created by Hakan Kjellerstrand (hakank@bonetmail.com)
Also, see his Comet page: http://www.hakank.org/comet 
'''

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def big_bang2(minimize_val=True):

    #
    # data
    #
    rock     = 0
    paper    = 1
    scissors = 2
    lizard   = 3
    spock    = 4

    m = 5   # number of dice
    n = 8   # number of faces of each die
    f = 2*n # max face value of dice 
    k = 10  # number of competitions
    edge =  [    
        [rock,scissors],
        [rock,lizard],
        [paper,rock],
        [paper,spock],
        [scissors,paper],
        [scissors,lizard],
        [lizard,paper],
        [lizard,spock],
        [spock,rock],
        [spock,scissors]
        ]

    #
    # declare variables
    #

    dice = intvar(1,f,shape=(m,n),name="dice")
    comp = intvar(0,n*n,shape=(k,2),name="comp")
    max_val = intvar(0, f, name="max_val")

    # objective
    if minimize_val == True:
        print("Minimizing max_val")
        model = Model(minimize=max_val)
    else:
        model = Model()


    #
    # constraints
    #
    model += (max_val == max(dice))

    # Symmetry breaking
    # order of the number of each die, lowest first
    model += [increasing(dice[i]) for i in range(m)]
    # This is faster but yield max_val = 11 instead of the correct 9!
    # model += [lex_less(dice[i],dice[i+1]) for i in range(m-1)]
    model += [dice[0,0] == 1]
    model += [AllDifferent([dice[i,0] for i in range(m)])]

    # and now we roll...
    for c in range(k):
        model += (comp[c,0] == sum([dice[edge[c][0],r1] > dice[edge[c][1],r2]
                                    for r1 in range(n) for r2 in range(n)]))
        
        model += (comp[c,1] == sum([dice[edge[c][1], r1] > dice[edge[c][0], r2]
                                    for r1 in range(n) for r2 in range(n)]))

        model += (comp[c,0] > comp[c,1])

        # added later by Thore:
        model += (comp[c,0] + comp[c,1] == n*n) # make sure there are no ties

    def print_sol():
        print("max_val:", max_val.value())
        print("dice:\n",dice.value())
        print("comp:\n",comp.value())
        print()
        
    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if minimize_val:
        ss.ort_solver.parameters.num_search_workers = 8        
        ss.solve()
        print_sol()
    else:
        num_solutions = ss.solveAll(display=print_sol)
        print()
        print("num_solutions:", num_solutions)
        
minimize_val = True  # Minimizing max value (0: no, 1: yes)
if len(sys.argv) > 1:
    minimize_val = int(sys.argv[1])

big_bang2(minimize_val)
