"""
Perfect squares problem in cpmpy.

CSPLib prob 009: Perfect square placements
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob009/index.html
'''
The perfect square placement problem (also called the squared square
problem) is to pack a set of squares with given integer sizes into a
bigger square in such a way that no squares overlap each other and all
square borders are parallel to the border of the big square. For a
perfect placement problem, all squares have different sizes. The sum of
the square surfaces is equal to the surface of the packing square, so
that there is no spare capacity. A simple perfect square placement
problem is a perfect square placement problem in which no subset of
the squares (greater than one) are placed in a rectangle.
'''

Problem instances:
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob009/results

Also see:
* Perfect square Dissection:
    http://mathworld.wolfram.com/PerfectSquareDissection.html

* http://en.wikipedia.org/wiki/Squaring_the_square

* http://squaring.net/index.html

* http://www.maa.org/editorial/mathgames/mathgames_12_01_03.html

* Global Constraint Catalog: squared squares:
    http://www.emn.fr/z-info/sdemasse/gccat/Ksquared_squares.html

This model has been heavily influenced by the Zinc model
discussed in

Kim Marriott, Nicholas Nethercote, Reza Rafeh,
Peter J. Stuckey, Maria Garcia de la Banda,
Mark Wallace
'The Design of the Zinc Modelling Language', page 7ff

All instances are solved in about 17s.


Here's the solution for problem6:
problem6
base: 30 sides: [1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 7, 8, 8, 9, 9, 10, 10, 11, 13]
Reversing sides to decreasing order...
Solution #1
s: [13, 11, 10, 10, 9, 9, 8, 8, 7, 5, 4, 3, 3, 2, 2, 1, 1, 1, 1]
x: [18, 1, 1, 12, 22, 1, 23, 10, 16, 11, 11, 13, 15, 11, 16, 15, 10, 12, 22]
y: [18, 1, 12, 1, 1, 22, 10, 23, 11, 14, 19, 11, 20, 12, 18, 19, 22, 11, 10]
Square:
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  6  6  6  6  6  6  6  6  6 
 2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3 17  8  8  8  8  8  8  8  8 
 2  2  2  2  2  2  2  2  2  2  2 14 14 10 10 10 10 10 11 11 11 11  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4 18 14 14 10 10 10 10 10 11 11 11 11  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4 12 12 12 10 10 10 10 10 11 11 11 11  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4 12 12 12 10 10 10 10 10 11 11 11 11  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4 12 12 12 10 10 10 10 10 16 13 13 13  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4  9  9  9  9  9  9  9 15 15 13 13 13  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4  9  9  9  9  9  9  9 15 15 13 13 13  8  8  8  8  8  8  8  8 
 4  4  4  4  4  4  4  4  4  4  9  9  9  9  9  9  9  1  1  1  1  1  1  1  1  1  1  1  1  1 
 4  4  4  4  4  4  4  4  4  4  9  9  9  9  9  9  9  1  1  1  1  1  1  1  1  1  1  1  1  1 
 4  4  4  4  4  4  4  4  4  4  9  9  9  9  9  9  9  1  1  1  1  1  1  1  1  1  1  1  1  1 
 4  4  4  4  4  4  4  4  4  4  9  9  9  9  9  9  9  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5 19  9  9  9  9  9  9  9  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
 5  5  5  5  5  5  5  5  5  7  7  7  7  7  7  7  7  1  1  1  1  1  1  1  1  1  1  1  1  1 
------

Square, alpha version:
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccfffffffff
bbbbbbbbbbbccccccccccphhhhhhhh
bbbbbbbbbbbnnjjjjjkkkkhhhhhhhh
ddddddddddqnnjjjjjkkkkhhhhhhhh
ddddddddddllljjjjjkkkkhhhhhhhh
ddddddddddllljjjjjkkkkhhhhhhhh
ddddddddddllljjjjjommmhhhhhhhh
ddddddddddiiiiiiiiimmmhhhhhhhh
ddddddddddiiiiiiiiimmmhhhhhhhh
ddddddddddiiiiiiiaaaaaaaaaaaaa
ddddddddddiiiiiiiaaaaaaaaaaaaa
ddddddddddiiiiiiiaaaaaaaaaaaaa
ddddddddddiiiiiiiaaaaaaaaaaaaa
eeeeeeeeeriiiiiiiaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
eeeeeeeeeggggggggaaaaaaaaaaaaa
------

status: ExitStatus.OPTIMAL (0.143951834 seconds)



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


#
# non_overlap
#
# Ensure that there are no overlaps of two rectangles
#
def non_overlap(s, t):
    (s_x, s_y, s_size) = s
    (t_x, t_y, t_size) = t
    return [( (s_x + s_size) <= t_x) |
            ( (t_x + t_size) <= s_x) |
            ( (s_y + s_size) <= t_y) |
            ( (t_y + t_size) <= s_y)
            ]


def perfect_squares(base,sides,num_sols=0):
    
    model = Model()

    # print unbuffered
    sys.stdout.flush()


    # Note: It seems to be faster if the sides are in
    #       _decreasing_ order. Ensure this.
    #       
    if sides[0] < sides[-1]:
        print("Reversing sides to decreasing order...")
        sides.reverse()


    num_sides = len(sides)

    # sanity check:
    # Ensure that the squares cover the base exactly
    assert sum(s * s for s in sides) == base * base, \
           "Squares do not cover the base exactly!"
    
    #
    # variables
    #
    squares = [(intvar(1, base),
                intvar(1, base),
                s)
               for s in sides]
    
    squares_flat = [sq[0]  for sq in squares] + [sq[1]  for sq in squares]

    
    squares_len = len(squares)

    #
    # constraints
    #
    for sq in squares:
        (s_x, s_y, s_size) = sq
        model += [s_x + s_size <= base + 1,
                  s_y + s_size <= base + 1];

    for i in range(squares_len):
        for j in range(squares_len):
            if i < j:
                model += [non_overlap(squares[i], squares[j])]
    
    model += [my_cumulative([squares[s][0] for s in range(squares_len)],
                  [squares[s][2] for s in range(squares_len)],
                  [squares[s][2] for s in range(squares_len)],
                  base)]

    model += [my_cumulative([squares[s][1] for s in range(squares_len)],
                   [squares[s][2] for s in range(squares_len)],
                   [squares[s][2] for s in range(squares_len)],
                   base)]

    def print_sol():
        fmt = "%%%ii" % len(str(base))
        alpha = "abcdefghijklmniopqrstuvwxyz"
        print("s:", [ squares[i][2] for i in range(squares_len) ])
        print("x:", [ squares[i][0].value() for i in range(squares_len) ])
        print("y:", [ squares[i][1].value() for i in range(squares_len) ])
        res = {}
        for i in range(1,base+1):
            for j in range(1,base+1):
                res[i,j] = 0

        #
        # Assign the squares in the main square.
        # Also checks:
        #   - that all cells are assigned
        #   - that we don't assign two squares
        #     in the same cell.
        ix = 1
        for sq in squares:
            (x,y,size) = sq
            x_val = x.value()
            y_val = y.value()
            for i in range(x_val, x_val+size):
                for j in range(y_val, y_val+size):
                    assert res[i,j] == 0, \
                           "res[%i,%i] should be 0: it is %i" \
                           % (i,j, res[i,j])
                    if res[i,j] > 0:
                        print("Strange: ", i,j, \
                              "is already occupied by", res[i,j], \
                              " will occupy with", ix)
                    res[i,j] = ix
            ix += 1

        #
        # Now check that all cells are assigned a value
        #
        for i in range(1,base+1):
            for j in range(1, base+1):
                assert res[i,j] > 0, "res[%i,%i] is == 0" % (i,j)

        #
        # Print the result
        #
        print("Square:")
        for i in range(1,base+1):
            for j in range(1, base+1):
                print(fmt % res[i,j],end= " ")
                # print("%s" % alpha[res[i,j]-1],end="")
            print()
        print("------\n")
        
        print("Square, alpha version:")
        for i in range(1,base+1):
            for j in range(1, base+1):
                # print(fmt % res[i,j],end= " ")
                print("%s" % alpha[res[i,j]-1],end="")
            print()
        print("------\n")
        

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print()
    print("num_solutions:", num_solutions)
    return ss.status()    

problems = {
    
    # First some easy problems:
    "problem1": {
    "base": 4,
    "sides" : [2,2,2,2]
    }, 

    "problem2": {
    "base" : 6,
     ##        1 2 3 4 5 6 7 8 9
    "sides" : [3,3,3,2,1,1,1,1,1]
    },

    # tricky problem: should give no solution
    # (we can not fit 2 3x3 squares in a 5x5 square)
    "problem3": {
    "base": 5,
    "sides" : [3,3,2,1,1,1]
    },

    # Problem from Sam Loyd
    # http://squaring.net/history_theory/sam_loyd.html
    "problem4": {
    "base" : 13,
    "sides" : [1, 1, 2, 2, 2, 3, 3, 4, 6, 6, 7]
    },
    
    # Problem from 
    # http://www.maa.org/editorial/mathgames/mathgames_12_01_03.html
    "problem5" : {
    "base" : 14,
    ## sides = [8,6,6,5,3,3,3,2,1,1,1,1]
    "sides" : [1,1,1,1,2,3,3,3,5,6,6,8]
    },


    # Problem from 
    # http://www.maa.org/editorial/mathgames/mathgames_12_01_03.html
    "problem6": {
    "base" : 30,
    "sides" : [1,1,1,1,2,2,3,3,4,5,7,8,8,9,9,10,10,11,13]
    },

    #
    # Harder problems:
    #
    "problem7" : {
    # #1,21,112,
    "base" : 112,
    "sides" : [2,4,6,7,8,9,11,15,16,17,18,19,24,25,27,29,33,35,37,42,50]
    },

    # 2,22,110,
    "problem8" : {
    "base" : 110,
    "sides" : [2,3,4,6,7,8,12,13,14,15,16,17,18,21,22,23,24,26,27,28,50,60]
    }

}

num_sols = 1
stats = []
for p in problems:
    print(p)
    base = problems[p]["base"]
    sides = problems[p]["sides"]
    print(f"base: {base} sides: {sides}")
    status = perfect_squares(base,sides,num_sols)
    stats.append(status)
    print()

print("stats:")
print(stats)
