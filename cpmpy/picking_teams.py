"""
Picking teams in cpmpy.

This model was inspired by David Curran's
blog post 'The Fairest Way to Pick a Team'
http://liveatthewitchtrials.blogspot.se/2012/06/fairest-way-to-pick-team.html
'''
What is the best way to pick a team? As kids we would always strictly alternate 
between teams so team 1 had first team 2 the second pick and then team 1 again etc.

Most things you can measure about people are on a bell curve. A small number of 
people are bad, most are in the middle and a few are good. There are a few good 
known metrics of ability. None are perfect, there is no one number that can sum up 
ability. The simpler the sport the more one metric can tell you, in cycling VO2 max is 
a very good indicator. Whereas in soccer VO2 max, kicking speed, vertical leap, number 
of keep me ups you can do etc could all measure some part of football ability.

So say there was one good metric for a task and teams were picked based on this. 
Is the standard strict alteration, where Team 1 picks then Team 2 alternating, fair? 
Fair here meaning both teams end up with a similar quality. 
'''
  

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math,random
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def picking_teams(s):
    n = len(s)
    n2 = math.ceil(n / 2)
    
    # print("s:",s)
    
    s_sum = sum(s)

    count = [n2,n2]

    print("s_sum:",s_sum)
    print("s_sum % 2:", s_sum % 2)

    x = intvar(1,2,shape=n,name="x")

    # the difference in strength between the teams
    # to be minimized
    d = intvar(0,s_sum,name="d")

    model = Model([d == abs(sum([s[i]*(x[i] == 1) for i in range(n)]) -
                            sum([s[i]*(x[i] == 2) for i in range(n)])
                            ),
                   ],
                 minimize=d
                  )

    # same size of team
    model += [sum([x[i] == 1  for i in range(n)]) == n2,
              sum([x[i] == 2  for i in range(n)]) == n2]
         

    # symmetry breaking: assign first person to team 1
    model += (x[0] == 1)

    # divisibility of the sum
    # model += ((s_sum % 2) == (d % 2)) # This throws an error!
    model += (int(s_sum % 2) == (d % 2))    

    def print_sol():
        xval = x.value()
        print("x:", xval)        
        print("d:", d.value())
        print("Team 1:", [i for i in range(n) if xval[i] == 1])
        print("Team 2:", [i for i in range(n) if xval[i] == 2])
        print(flush=True)

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(display=print_sol)
    print()
    print("number of solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()

s = [35, 52, 17, 26, 90, 55, 57, 54, 41, 9, 75, 24, 17, 23, 62, 74, 100, 67, 40, 48, 7, 6, 44, 19, 16, 14, 2, 66, 70, 2, 43, 45, 76, 53, 90, 12, 88, 96, 30, 30, 36, 93, 74, 1, 52, 45, 38, 7, 24, 96, 17, 21, 12, 12, 23, 90, 77, 64, 37, 79, 67, 62, 24, 11, 74, 82, 51, 17, 72, 18, 37, 94, 43, 44, 32, 86, 94, 33, 97, 27, 38, 38, 29, 92, 35, 82, 22, 66, 80, 8, 62, 72, 25, 13, 94, 42, 51, 31, 69, 66]
picking_teams(s)

# Randomize s
n = 1000
s = [random.randint(1,100) for _ in range(n)]
picking_teams(s)

