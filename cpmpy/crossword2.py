"""
Crosswords in cpmpy.

This is a standard example for constraint logic programming. See e.g.
http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
'''
We are to complete the puzzle

     1   2   3   4   5
   +---+---+---+---+---+       Given the list of words:
 1 | 1 |   | 2 |   | 3 |             AFT     LASER
   +---+---+---+---+---+             ALE     LEE
 2 | # | # |   | # |   |             EEL     LINE
   +---+---+---+---+---+             HEEL    SAILS
 3 | # | 4 |   | 5 |   |             HIKE    SHEET
   +---+---+---+---+---+             HOSES   STEER
 4 | 6 | # | 7 |   |   |             KEEL    TIE
   +---+---+---+---+---+             KNOT
 5 | 8 |   |   |   |   |
   +---+---+---+---+---+       
 6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
   +---+---+---+---+---+       puzzle correspond to the words 
                               that will start at those locations.
'''

Answer:
[0, 2, 4, 6, 7, 11, 13, 1]
h o s e s
l a s e r
s a i l s
s h e e t
s t e e r
h e e l _
h i k e _
k e e l _



The model was inspired by Sebastian Brand's Array Constraint cross word example
http://www.cs.mu.oz.au/~sbrand/project/ac/
http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *
from collections import defaultdict

def print_solution(A, E, alpha, n, word_len):
    for ee in range(n):
        print(f"{ee:2d}: ({E[ee].value():2d})", end=" ")
        for ii in range(word_len):
            print(alpha[A[ee,ii].value()],end=" ")
        print()

def crossword():

    alpha = "_abcdefghijklmnopqrstuvwxyz";

    a=1;  b=2;  c=3;  d=4;  e=5;  f=6;  g=7;  h=8;  i=9;
    j=10; k=11; l=12; m=13; n=14; o=15; p=16;
    q=17; r=18; s=19; t=20; u=21; v=22; w=23;
    x=24; y=25; z=26;
    
    num_words = 15
    word_len = 5;
    AA = [
            [h, o, s, e, s], #  HOSES
            [l, a, s, e, r], #  LASER
            [s, a, i, l, s], #  SAILS
            [s, h, e, e, t], #  SHEET
            [s, t, e, e, r], #  STEER
            [h, e, e, l, 0], #  HEEL
            [h, i, k, e, 0], #  HIKE
            [k, e, e, l, 0], #  KEEL
            [k, n, o, t, 0], #  KNOT
            [l, i, n, e, 0], #  LINE
            [a, f, t, 0, 0], #  AFT
            [a, l, e, 0, 0], #  ALE
            [e, e, l, 0, 0], #  EEL
            [l, e, e, 0, 0], #  LEE
            [t, i, e, 0, 0]  #  TIE
        ]

    num_overlapping = 12
    overlapping = [
        [0, 2, 1, 0],   #  s
        [0, 4, 2, 0],   #  s 
    
        [3, 1, 1, 2],   #  i
        [3, 2, 4, 0],   #  k
        [3, 3, 2, 2],   #  e
        
        [6, 0, 1, 3],   #  l
        [6, 1, 4, 1],   #  e
        [6, 2, 2, 3],   #  e
        
        [7, 0, 5, 1],   #  l
        [7, 2, 1, 4],   #  s
        [7, 3, 4, 2],   #  e
        [7, 4, 2, 4]    #  r
        ]

    A = intvar(0,26,shape=(num_words, word_len),name="A");

    n = 8
    E = intvar(0, num_words,shape=n,name="E")

    model = Model()

    for I in range(num_words):
        for J in range(word_len):
            model += [A[I,J] == AA[I][J]]
    
    model += [AllDifferent(E)]
              
    for I in range(num_overlapping):
        model += [A[E[overlapping[I][0]], overlapping[I][1]] ==  A[E[overlapping[I][2]], overlapping[I][3]]]

    def print_sol():
        print(E.value())
        print_solution(A,E,alpha, n, word_len)


    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)    
    print("\nnumber of solutions:", num_solutions)


crossword()
