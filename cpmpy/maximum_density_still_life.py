"""
Maximum density still life in cpmpy.

CSPLib 032: http://www.csplib.org/prob/prob032
'''
Proposed by Barbara Smith

This problem arises from the Game of Life, invented by John Horton Conway in the 1960s and
popularized by Martin Gardner in his Scientific American columns.

Life is played on a squared board, considered to extend to infinity in all directions. Each
square of the board is a cell, which at any time during the game is either alive or dead.
A cell has eight neighbours:

Magic Hexagon
The configuration of live and dead cells at time t leads to a new configuration at time
t+1 according to the rules of the game:

* if a cell has exactly three living neighbours at time t, it is alive at time t+1
* if a cell has exactly two living neighbours at time t it is in the same state at time
  t+1 as it was at time t
* otherwise, the cell is dead at time t+1

A stable pattern, or still-life, is not changed by these rules. Hence, every cell that
has exactly three live neighbours is alive, and every cell that has fewer than two or
more than three live neighbours is dead. (An empty board is a still-life, for instance.)

What is the densest possible still-life pattern, i.e. the pattern with the largest
number of live cells, that can be fitted into an n x n section of the board, with all the
rest of the board dead?

(Note that another definition of a still-life requires the pattern to be a single object -
see for instance Mark Niemiec’s Definitions of Life Terms page. On this definition,
the 8 x 8 pattern below is a pseudo still-life.)
'''

This model (or rather my earlier MiniZinc and Comet models)
was inspired by the OPL model by
Toni Mancini, Davide Micaletto, Fabio Patrizi, Marco Cadoli: 
'Evaluating ASP and commercial solvers on the CSPLib'
http://www.dis.uniroma1.it/~tmancini/index.php?problemid=032&solver=OPL&spec=BASE&currItem=research.publications.webappendices.csplib2x.problemDetails#listing

(The comments of the constraints etc are from this as well.)


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations


def maximum_density_still_life(size=6,num_procs=1):

    grid_size = size+4

    # Search space: The set of all possible assignments of 0s (dead) and 1s (live) 
    # to the cells of the board section. However, to be able to easily express 
    # constraints on "boundary" cells, we take as search space the set of 0/1 
    # boards of size n+4 by n+4: the actual stable pattern appears in the sub-board 
    # defined by ignoring the first/last two rows/columns.
    grid = boolvar(shape=(grid_size,grid_size),name="grid")

    # Objective function: Maximize the number of live cells in the sub-board defined 
    # by ignoring the first/last two/ rows/columns.

    z = intvar(0,grid_size**2,name="z")

    model = Model([z == sum([grid[i,j] for i in range(2,size+1+1) for j in range(2,size+1+1)])])
    
    # C1: Cells in the first/last two rows/columns are all 0 (dead)
    for i in range(grid_size):
        model += (grid[0,i] == 0)
        model += (grid[1,i] == 0)
        model += (grid[size+2,i] == 0)  
        model += (grid[size+3,i] == 0)
        model += (grid[i,0] == 0)       
        model += (grid[i,1] == 0)
        model += (grid[i,size+2] == 0)  
        model += (grid[i,size+3] == 0)
  
    for r in range(1,size+2+1):
        for c in range(1,size+2+1):
            # C2: Each cell of the board (except those of the first/last row/column) 
            #     that has exactly three live neighbors is alive. 
            #     Together with constraint C1, this implies that cells in the
            #     second/last-but-one row/column cannot have three live neighbors.
            model += (( ( grid[r-1,c-1] + grid[r-1,c] + grid[r-1,c+1] + 
                          grid[r,c-1] + grid[r,c+1] + 
                          grid[r+1,c-1] + grid[r+1,c] + grid[r+1,c+1]
                          ) == 3 
                        ).implies(grid[r,c] == 1))
    
            # C3: Each live cell must have 2 or 3 live neighbors (cells of the first/last 
            # row/column may be ignored by this constraint)
            model += ((grid[r,c] == 1).implies(
                        (2 <= 
                        ( grid[r-1,c-1] + grid[r-1,c] + grid[r-1,c+1] +
                          grid[r,c-1] + grid[r,c+1] +
                          grid[r+1,c-1] + grid[r+1,c] + grid[r+1,c+1] 
                          ))
                        &
                        (( grid[r-1,c-1] + grid[r-1,c] + grid[r-1,c+1] +
                          grid[r,c-1] + grid[r,c+1] +
                          grid[r+1,c-1] + grid[r+1,c] + grid[r+1,c+1] 
                          ) <= 3)
                                ))
  

    # SBSO: Symmetry-breaking by selective ordering
    # The assignment is forced to respect an ordering on the values that occur in corner entries
    # of the board. In particular:  
    # - if the NW-corner cell is dead, the SE-corner cell
    # must be dead too 
    # - if the NE-corner cell is dead, the SW-corner cell must be dead too
    # 
    model += (grid[2,2] >= grid[size+1,size+1])
    model += (grid[2,size+1] >= grid[size+1,2])

    model.maximize(z)

    num_solutions = 0
    ss = CPM_ortools(model)
    ss.ort_solver.parameters.num_search_workers = num_procs # Don't work together with SearchForAllSolutions
    ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    if ss.solve():
        num_solutions += 1
        print("z:",z.value())
        print("grid:")
        print(grid.value())
        print()
        print("Num conflicts:", ss.ort_solver.NumConflicts())
        print("NumBranches:", ss.ort_solver.NumBranches())
        print("WallTime:", ss.ort_solver.WallTime())
        
        print()

    print("number of solutions:", num_solutions)

num_procs = 14
for size in range(1,15):
    print("\nsize:",size)
    maximum_density_still_life(size,num_procs)
