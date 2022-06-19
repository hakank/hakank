"""
Five brigands problem in cpmpy.

From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
'''
The Five Brigands from 'Amusements in Mathematics, Dudeney',
number 133.

The five Spanish brigands, Alfonso, Benito, Carlos, Diego, and Esteban,
were counting their spoils after a raid, when it was found that they
had captured altogether exacly 200 doubloons. One of the band pointed
out that if Alfonso had twelve times as much, Benito three times as
much, Carlos the same amount, Diego half as much, and Esteban one-
third as much, they would still have altogether just 200 doubloons.
How many doubloons had each?

There are a good many equally correct answers to this problem. The
puzzle is to discover exactly how many different answers there are, it
being understood that every man had something and there is to be no
fractional money. 
'''

There are 45 different solutions:
#1:
[ 8  8  8 40 32]

#2:
[ 8  9  8 32 37]

#3:
[ 8  9  9 30 38]

#4:
[ 8  8  9 38 33]

#5:
[ 8 10  9 22 43]

#6:
[ 8 10  8 24 42]
....

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def five_brigands():
    # Everybody has at least 8d.; nobody has more than 160    
    x = intvar(8,160,shape=5,name="x")
    A,B,C,D2,E3 = x
    model = Model([
        A + B + C + 2*D2 + 3*E3 == 200,
        A * 12 + B * 3 + C + D2 + E3 == 200,
        D2 <= 100,
        E3 <= 66,
        ])

    num_solutions = model.solveAll(display=x)
    print("num_solutions:",num_solutions)

#
# Show the possible values for each brigands over all 45 solutions.
#
def five_brigands2():
    # Everybody has at least 8d.; nobody has more than 160
    n = 5
    x = intvar(8,160,shape=n,name="x")
    A,B,C,D2,E3 = x
    model = Model([
        A + B + C + 2*D2 + 3*E3 == 200,
        A * 12 + B * 3 + C + D2 + E3 == 200,
        D2 <= 100,
        E3 <= 66,
        ])

    
    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0


    d = {}
    for i in range(5):
        d[i] = []

    def print_sol():
        xval = x.value()
        print("x:", xval)
        for i in range(n):
            if not xval[i] in d[i]:
                d[i].append(xval[i])
        
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)
    print("Possible values for each brigands:")
    for i in range(5):
        print(f"{i}: {sorted(d[i])}")

five_brigands()
print()
five_brigands2()
