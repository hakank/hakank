"""
Magic Square Water Retention in cpmpy.

This is inspired by the Numberjack example MagicWater.py
'''
The goal of the Magic Square Water Retention problem is to retain the maximum
amount of water in a N x N magic square.
'''

http://www.knechtmagicsquare.paulscomputing.com/topographical.html
''
a) there are 880 different order 4 magic squares
b) 137 of the 880 squares retain no water according to the topographical model
'''


Also see:
  - http://www.knechtmagicsquare.paulscomputing.com/
  - http://www.knechtmagicsquare.paulscomputing.com/topographical.html
  - http://en.wikipedia.org/wiki/Associative_magic_square
  - http://en.wikipedia.org/wiki/Water_retention_on_mathematical_surfaces
  - http://www.azspcs.net/Contest/MagicWater
  - Johan Öfverstedt's CBLS solver (C++ and Comet): http://sourceforge.net/projects/wrmssolver/
     


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations



def magic_square_water_retension(n,num_sols=0):

    total = math.floor((n*(n*n+1)) / 2)
    assoc = n*n+1

 
    magic = intvar(1,n*n,shape=(n,n),name="magic")
    water = intvar(1,n*n,shape=(n,n),name="water")

    
    # the difference between water and magic
    # diffs = intvar(0,n*n,shape=(n,n),name="water")

    # objective (to maximize)
    z = intvar(0,n*n*n,name="z")

    v = math.floor((n*n*(n*n+1)) / 2)
    model = Model([AllDifferent(magic),
                  z == sum([water[i,j] for i in range(n) for j in range(n)]) - v,
                  ])
    
    # water retention
    # This is from the Numberjack model (magicwater.py)
    # first, the rim
    for i in range(n):
        # rows
        model += [water[i,0] == magic[i,0],
                  water[i,n-1] == magic[i,n-1],
        
                  # columns
                  water[0,i] == magic[0,i],
                  water[n-1,i] == magic[n-1,i]
                  ]
  
    # then the inner cells (max between their own height and of 
    # the water level around)
    for a in range(1,n-1):
        for b in range(1,n-1):
            model += [water[a,b] == max([magic[a,b], min([water[a-1,b], water[a,b-1], 
                                        water[a+1,b], water[a,b+1]])])]

    for i in range(n):
        for j in range(n):
            model += [water[i,j] >= magic[i,j]]

    for k in range(n):
        model += [sum([magic[k,i] for i in range(n)]) == total,
                  sum([magic[i,k] for i in range(n)]) == total
                  ]

    # diagonal 1
    model += [sum([magic[i,i] for i in range(n)]) == total]
    # diagonal 2
    model += [sum([magic[i,n-i-1] for i in range(n)]) == total]

    # "associative value"
    for i in range(n):
        for j in range(n):
            model += [magic[i,j] + magic[n-i-1,n-j-1] == assoc]
 
    #
    # Symmetry breaking: 
    # Frénicle standard form
    model += [magic[0,0] == min([magic[0,0], magic[0,n-1], magic[n-1,0], magic[n-1,n-1]]),
              magic[0,1] < magic[1,0]
              ]

    model.maximize(z)

    num_solutions = 0
    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    if ss.solve():
        num_solutions += 1
        print("z:",z.value())
        print("magic:")
        magic_val = magic.value()
        print(magic_val)
        print("water:")
        water_val = water.value()
        print(water_val)
        print("diffs:")
        print(np.array(water_val-magic_val))
        print()
        print("Num conflicts:", ss.ort_solver.NumConflicts())
        print("NumBranches:", ss.ort_solver.NumBranches())
        print("WallTime:", ss.ort_solver.WallTime())
        
        print()

    print("number of solutions:", num_solutions)


for n in range(3,10):
    print("\nn:",n)
    magic_square_water_retension(n)
