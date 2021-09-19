"""
Lichtenstein coloring problem in cpmpy.

From 
http://bit-player.org/2008/the-chromatic-number-of-liechtenstein
and
'''
It seems that Liechtenstein is divided into 11 communes, which 
emphatically do not satisfy the connectivity requirement of the four 
color map theorem. Just four of the communes consist of a single 
connected area (Ruggell, Schellenberg and Mauren in the north, and 
Triesen in the south). 
...
In the map above, each commune is assigned its own color, and so we 
have an 11-coloring. It’s easy to see we could make do with fewer 
colors, but how many fewer? I have found a five-clique within the map; 
that is, there are five communes that all share a segment of border 
with one another. It follows that a four-coloring is impossible. Is 
there a five-coloring? What is the chromatic number of Liechtenstein?
'''

Also see
http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def liechtenstein_coloring():

    # communes
    BalzersC = 0
    EschenC = 1
    GamprinC = 2
    MaurenC = 3
    PlankenC = 4
    RuggellC = 5
    SchaanC = 6
    SchellenbergC = 7
    TriesenC = 8
    TriesenbergC = 9
    VaduzC = 10

    # enclaves/exclaves
    Balzers1 = 0
    Balzers2 = 1
    Balzers3 = 2
    Eschen1 = 3
    Eschen2 = 4
    Eschen3 = 5
    Gamprin1 = 6
    Gamprin2 = 7
    Mauren = 8
    Planken1 = 9
    Planken2 = 10
    Planken3 = 11
    Planken4 = 12
    Ruggell = 13
    Schaan1 = 14
    Schaan2 = 15
    Schaan3 = 16
    Schaan4 = 17
    Schaan5 = 18
    Schellenberg = 19
    Triesen = 20
    Triesenberg1 = 21
    Triesenberg2 = 22
    Vaduz1 = 23
    Vaduz2 = 24
    Vaduz3 = 25
    Vaduz4 = 26
    Vaduz5 = 27
    Vaduz6 = 28
    
    
    num_communes = 11
    num_colors = 11 # num_communes # 6
    num_edges = 45
    num_enclaves = 29
    
    # the enclaves and corresponding commune
    cc = [BalzersC, BalzersC, BalzersC, 
          EschenC, EschenC, EschenC, 
          GamprinC, GamprinC, 
          MaurenC, 
          PlankenC, PlankenC, PlankenC, PlankenC, 
          RuggellC, 
          SchaanC, SchaanC, SchaanC, SchaanC, SchaanC, 
          SchellenbergC, 
          TriesenC, 
          TriesenbergC, TriesenbergC, 
          VaduzC, VaduzC, VaduzC, VaduzC, VaduzC, VaduzC
          ]

    # neighbours
    Liechtenstein = cpm_array([[Ruggell, Schellenberg],
                     [Ruggell, Gamprin1],
                     [Schellenberg, Mauren],
                     [Schellenberg, Eschen1],
                     [Mauren, Eschen1],
                     [Gamprin1, Eschen2],
                     [Gamprin1, Vaduz2],
                     [Gamprin1, Schaan1],
                     [Gamprin1, Planken3],
                     [Gamprin1, Eschen1],
                     [Eschen1, Gamprin2],
                     [Eschen1, Planken1],
                     [Eschen2, Schaan1],
                     [Vaduz3, Schaan1],
                     [Vaduz2, Schaan1],
                     [Planken3, Schaan1],
                     [Planken2, Schaan1],
                     [Schaan1, Planken1],
                     [Schaan1, Planken4],
                     [Schaan1, Vaduz1],
                     [Gamprin2, Eschen3],
                     [Eschen3, Vaduz4],
                     [Eschen3, Schaan2],
                     [Vaduz4, Schaan2],
                     [Vaduz4, Planken1],
                     [Schaan2, Planken1],
                     [Planken1, Schaan3],
                     [Vaduz1, Triesenberg1],
                     [Vaduz1, Triesen],
                     [Planken4, Triesenberg1],
                     [Planken4, Balzers2],
                     [Balzers2, Vaduz5],
                     [Balzers2, Schaan4],
                     [Vaduz5, Schaan4],
                     [Schaan4, Triesenberg1],
                     [Schaan4, Vaduz6],
                     [Schaan4, Triesenberg2],
                     [Triesenberg1, Vaduz6],
                     [Triesenberg1, Triesen],
                     [Triesenberg1, Balzers3],
                     [Triesen, Balzers3],
                     [Triesen, Balzers1],
                     [Triesen, Schaan5],
                     [Vaduz6, Schaan5],
                     [Triesenberg2, Schaan5]
                     ])

    # colors for the en-/exclaves
    color = intvar(0,num_communes-1,shape=num_enclaves,name="color")

    # colors for the communes
    color_communes = intvar(0,num_colors-1,shape=num_communes,name="color_communes")

    # what colors are used (for minimizing number of colors)
    color_used = boolvar(shape=num_colors,name="color_used")

    # number of colors used
    n_colors = intvar(0,num_communes,name="n_colors")
    max_color_used = intvar(0,num_colors-1,name="max_color_used")    

    # solve minimize n_colors
    # model = Model(minimize=n_colors)
    model = Model(minimize=max_color_used)

    # constraints
    model += (n_colors == sum(color_used))
    model += (max_color_used == max(color))    

    # connect color_used <-> color
    for i in range(num_colors):
        model += ((color_used[i] == 1) == (sum([color[c] == i for c in range(num_enclaves)]) > 0))

    # all neighbours must have different colors
    for i in range(num_edges):
        model += (color[Liechtenstein[i,0]] != color[Liechtenstein[i,1]])
      
    # first commune (Balzers) has color 0
    model += (color_communes[Balzers1] == 0)

    # exclaves of the same commune must have the same color
    for i in range(num_enclaves):
        for j in range(num_enclaves):
            if i != j and cc[i] == cc[j]:
                model += (color[i] == color[j])
   
    # connection between commune and en-/exclaves
    for c in range(num_communes):
        model += (sum([(color_communes[c] == color[e]) & (cc[e] == c)
                       for e in range(num_enclaves)]) > 0)

    ss = CPM_ortools(model)
    num_solutions = 0
    if ss.solve():
        num_solutions += 1
        print("n_colors:", n_colors.value())
        print("max_color_used:", max_color_used.value())                
        print("color_used:", color_used.value())        
        print("color:", color.value())
        print("color_communes:", color_communes.value())
        print("status:", ss.status())
        print()

liechtenstein_coloring()
