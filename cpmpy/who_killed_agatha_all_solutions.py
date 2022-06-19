#!/usr/bin/python3
"""
Who killed agatha? problem in CPMpy

This is based on the CPMpy's example examples/who_killed_agatha.py
'''
Based on the my Numberjack model of Hakan Kjellerstrand
see also: http://www.hakank.org/constraint_programming_blog/2014/11/decision_management_community_november_2014_challenge_who_killed_agath.html
'''

This version shows all (8) solutions of the problem, all indicating
that Agatha is the killer.

This CPMpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my CPMpy page: http://hakank.org/cpmpy/
"""
from cpmpy import *
from enum import Enum
import numpy as np
from cpmpy.solvers import CPM_ortools
from cpmpy_hakank import *

def who_killed_agatha():

    # Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
    # are the only ones to live there. 
    n = 3
    who = ["agatha","butler","charles"]
    (agatha, butler, charles) = range(n) # enum constants

    # Who killed agatha?
    victim = agatha
    killer = IntVar(0,2,name="killer")

    model = Model()
    # A killer always hates, and is no richer than his victim. 
    hates = BoolVar((n,n),name="hates")
    model += [ hates[killer, victim] == 1 ]

    richer = BoolVar((n,n),name="richer")
    model += [ richer[killer, victim] == 0 ]
    
    # implied richness: no one richer than himself, and anti-reflexive
    model += [ richer[i,i] == 0 for i in range(n) ]
    model += [ (richer[i,j] == 1) == (richer[j,i] == 0) for i in range(n) for j in range(n) if i != j ]
    
    # Charles hates noone that Agatha hates. 
    model += [ (hates[agatha,i] == 1).implies(hates[charles,i] == 0) for i in range(n) ]

    # Agatha hates everybody except the butler. 
    model += [hates[agatha,(agatha,charles,butler)] == [1,1,0]]
    # model += [ hates[agatha,agatha]  == 1,
    #                 hates[agatha,charles] == 1,
    #                 hates[agatha,butler]  == 0 ]

    # The butler hates everyone not richer than Aunt Agatha. 
    model += [ (richer[i,agatha] == 0).implies(hates[butler,i] == 1) for i in range(n) ]

    # The butler hates everyone whom Agatha hates. 
    model += [ (hates[agatha,i] == 1).implies(hates[butler,i] == 1) for i in range(n) ]

    # No one hates everyone. 
    model += [ sum([hates[i,j] for j in range(n)]) <= 2 for i in range(n) ]

    print("Model:")
    print(model)
    print()

    def print_sol():
        print("killer:",who[killer.value()])
        
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)


who_killed_agatha()
