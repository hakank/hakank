"""
Music men puzzle in cpmpy.

From
http://groups.google.com/groups?q=FINITE+DOMAINS+With+Logic+Puzzles&hl=en&lr=&ie=UTF-8&c2coff=1&safe=off&selm=1992Jul27.034607.19386#40IRO.UMontreal.CA&rnum=4
'''
MUSIC MEN

Three friends like different kinds of music.  From the clues given
below, can you identify them, say how old each is, and work out
his musical preference?
#
Clues: 
1.      Rob is older than Queen, who likes classical music.
2.      The pop-music fan, who is not Prince, is not 24.
3.      Leon, who is not King, is 25.
4.      Mark's musical preference is not jazz.

Knowledge: "this is what we know of the world."
Names           : Leon, Mark, Rob.
Surnames        : King, Prince, Queen.
Ages            : 24, 25, 26.
Music           : Classical, Jazz, Pop.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def music_men():

    n = 3
    min_age = 24
    max_age = 26

    # variables
    Age24 = 24
    Age25 = 25
    Age26 = 26
    Age  = [Age24, Age25, Age26];
    
    Names  = intvar(min_age,max_age,shape=n,name="Names")
    King, Prince, Queen = Names
    
    Surnames = intvar(min_age,max_age,shape=n,name="Surnames")
    Leon, Mark, Rob = Surnames
    
    Music    = intvar(min_age,max_age,shape=n,name="Music")
    Classical, Jazz, Pop = Music

    # constraints
    model = Model([
        AllDifferent(Names),
        AllDifferent(Surnames),
        AllDifferent(Music),
    
        # Rob is older than Queen  who likes classical music.
        Rob > Queen,
        Queen == Classical,

        # The pop-music fan  who is not Prince  is not 24.
        Pop != Prince,
        Pop != Age24,

        # Leon  who is not King  is 25.
        Leon != King,
        Leon == Age25,

        #  Mark's musical preference is not jazz.
        Mark != Jazz,
        ])

    def print_sol():
        print("Names   :", Names.value())
        print("Surnames:", Surnames.value())
        print("Music   :", Music.value())
        print()


    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)



music_men()
