#
# Working together problem in z3.
#
# Marriott & Stuckey Programming in Constraint, page 191ff,
# Temperature in a grid
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3 import *

def finite_element(init_grid):
    s = Solver()

    rows = len(init_grid)
    cols = len(init_grid[0])

    grid = [ [Real(f"grid[{i},{j}") for j in range(cols)] for i in range(rows)]
    for i in range(rows):
        for j in range(cols):
            if init_grid[i][j] >= 0:
                s.add(grid[i][j] == init_grid[i][j])

    for i in range(1,rows-1):
        for j in range(1,cols-1):
            s.add(grid[i][j] >= 0)            
            s.add(grid[i][j] == (grid[i][j-1] + grid[i-1][j] + grid[i+1][j] + grid[i][j+1]) / 4)


    if s.check() == sat:
        mod = s.model()
        for i in range(rows):
            for j in range(cols):
                if init_grid[i][j] < 0:
                    t = f"{1.0*mod[grid[i][j]].as_fraction():3.2f}"
                else:
                    t = f"{init_grid[i][j]:3.2f}"
                print(t,end=" ")
            print()
        print()

T = -1
grid = [[0.0, 100.0, 100.0, 100.0, 100.0, 100.0, 0.0],
        [0.0,   T,       T,     T,     T,     T, 0.0],
        [0.0,   T,       T,     T,     T,     T, 0.0],
        [0.0,   T,       T,     T,     T,     T, 0.0],
        [0.0,   T,       T,     T,     T,     T, 0.0],
        [0.0, 0.0,     0.0,   0.0,    0.0,  0.0, 0.0]
        ]


finite_element(grid)
