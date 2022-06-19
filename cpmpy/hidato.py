"""
Hidato puzzle in cpmpy.
  
http://www.shockwave.com/gamelanding/hidato.jsp
http://www.hidato.com/
'''
Puzzles start semi-filled with numbered tiles.
The first and last numbers are circled.
Connect the numbers together to win. Consecutive
number must touch horizontally, vertically, or
diagonally.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



#
# Solve the Hidato problem
#
def hidato(problem,solver="CPM_ortools"):

    r = problem["r"]
    c = problem["c"]
    puzzle = problem["puzzle"]

    print_game(puzzle, r,c)

    #
    # Decision variables
    # Note: Matrix is defined with cols,rows,...
    #
    x = intvar(1,r*c,shape=(c,r),name="x")
    # We have to convert to a flattened version
    # since element only support 1 dimension arrays
    x_flat = [x[i,j] for i in range(r) for j in range(c)]


    model = Model(
                 AllDifferent(x)
                 )

    #
    # Fill in the clues
    #
    for i in range(r):
        for j in range(c):           
            if puzzle[i][j] > 0:
                model += [x[i,j] == puzzle[i][j]]

    # From the numbers k = 1 to r*c-1, find this position,
    # and then the position of k+1
    for k in range(1,r*c):
        i = intvar(0,r)
        j = intvar(0,c)
        a = intvar(-1,1)
        b = intvar(-1,1)
        
        # 1) First: fix "this" k
        # 2) and then find the position of the next value (k+1)
        model +=[
            # k == x[i,j], # This don't work now
            k == Element(x_flat,i*c+j), # This works
            
            # k + 1 == x[i+a,j+b]
            k + 1 == Element(x_flat,(i+a)*c+j+b)
            ]

        model += [
            i+a >= 0,
            j+b >= 0,
            i+a < r,
            j+b < c,
            ((a != 0) | (b != 0))
            ] 

    def print_sol():
        print_board(x, r, c)

    num_solutions = 0
    if solver == "CPM_ortools":
        ss = CPM_ortools(model)
        # ss.ort_solver.parameters.linearization_level = 0
        # ss.ort_solver.parameters.cp_model_probing_level = 0  
        num_solutions = ss.solveAll(display=print_sol)
        print("number of solutions:", num_solutions)
        print("Num conflicts:", ss.ort_solver.NumConflicts())
        print("NumBranches:", ss.ort_solver.NumBranches())
        print("WallTime:", ss.ort_solver.WallTime())
        print()

    else:
        ss = CPM_minizinc(model)
        # ss = CPM_minizinc(model,"gecode") # Test a specific FlatZinc model
        # print("MiniZinc model:\n",ss.make_model(model)[0]) # Print the MiniZinc model
        num_solutions = ss.solveAll(display=print_sol)


    assert num_solutions > 0, "The number of solutions should be > 0!"
        
#
# Print the mines
#
def print_board(x, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print("% 3s" % x[i,j].value(),end=" ")
        print()
    print("\n")
    print(flush=True)

def print_game(game, rows, cols):
    for i in range(rows):
        for j in range(cols):
            print("% 3s" % game[i][j],end=" ")
        print()
    print(flush=True)


problems = {
    # Simple problem
    "simple1": {
    "r": 3,
    "c": 3,
    "puzzle": [ 
        [6,0,9],
        [0,2,8],
        [1,0,0]
        ]
    },

    "misc1": {
    "r": 7,
    "c": 7,
    "puzzle": [ 
        [0,44,41, 0, 0, 0, 0],
        [0,43, 0,28,29, 0, 0],
        [0, 1, 0, 0, 0,33, 0],
        [0, 2,25, 4,34, 0,36],
        [49,16, 0,23, 0, 0, 0],
        [0,19, 0, 0,12, 7, 0],
        [0, 0, 0,14, 0, 0, 0] 
        ]
    },

    # Problems from the book:
    # Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"
    #
    # Problem 1 (Practice)
    "problem1": {
    "r": 5,
    "c": 5,
    "puzzle": [
       [ 0, 0,20, 0, 0],
       [ 0, 0, 0,16,18],
       [22, 0,15, 0, 0],
       [23, 0, 1,14,11],
       [ 0,25, 0, 0,12],
       ]
    },


    # problem 2 (Practice)
    "problem2": {
    "r": 5,
    "c": 5,
    "puzzle": [
        [0, 0, 0, 0,14],
        [0,18,12, 0, 0],
        [0, 0,17, 4, 5],
        [0, 0, 7, 0, 0],
        [9, 8,25, 1, 0],
        ]
    },

    # problem 3 (Beginner)
    "problem3": {
    "r": 6,
    "c": 6,
    "puzzle": [
        [ 0, 26,0, 0, 0,18],
        [ 0, 0,27, 0, 0,19],
        [31,23, 0, 0,14, 0],
        [ 0,33, 8, 0,15, 1],
        [ 0, 0, 0, 5, 0, 0],
        [35,36, 0,10, 0, 0]
        ]
    },


    # Problem 15 (Intermediate)
    "problem15" : {
    "r": 8,
    "c": 8,
    "puzzle": [
         [64, 0, 0, 0, 0, 0, 0, 0],
         [ 1,63, 0,59,15,57,53, 0],
         [ 0, 4, 0,14, 0, 0, 0, 0],
         [ 3, 0,11, 0,20,19, 0,50],
         [ 0, 0, 0, 0,22, 0,48,40],
         [ 9, 0, 0,32,23, 0, 0,41],
         [27, 0, 0, 0,36, 0,46, 0],
         [28,30, 0,35, 0, 0, 0, 0]
         ]
    },

    # Problem 156 (Master}
    "problem156": {
    "r": 10,
    "c": 10,
    "puzzle": [
               [88, 0, 0,100, 0, 0,37,0, 0,34],
               [ 0,86, 0,96,41, 0, 0,36, 0, 0],
               [ 0,93,95,83, 0, 0, 0,31,47, 0],
               [ 0,91, 0, 0, 0, 0, 0,29, 0, 0],
               [11, 0, 0, 0, 0, 0, 0,45,51, 0],
               [ 0, 9, 5, 3, 1, 0, 0, 0, 0, 0],
               [ 0,13, 4, 0, 0, 0, 0, 0, 0, 0],
               [15, 0, 0,25, 0, 0,54,67, 0, 0],
               [ 0,17, 0,23, 0,60,59, 0,69, 0],
               [19, 0,21,62,63, 0, 0, 0, 0, 0]
               ]
    },


    # Problem 188 (Genius]
    "problem188": {
    "r": 12,
    "c": 12,
    "puzzle": [[  0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0],
               [136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0],
               [139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0],
               [  0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103],
               [  0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101],
               [  0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0],
               [ 30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0],
               [ 32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92],
               [  0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0],
               [  0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0],
               [ 35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0],
               [ 36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88]
               ]
    },


    # # From NFZ
    # #  https://groups.google.com/g/picat-lang/c/899o44qEPZQ/m/5Yf9vlKLAgAJ
    # "problem_nfz" : {
    # "r" : 10,
    # "c" : 10,
    # "puzzle" : [[  1,0,0,0,0, 0,0,0,0, 0],
    #             [ 20,0,0,0,0,15,0,0,0,11],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [  0,0,0,0,0, 0,0,0,0, 0],
    #             [100,0,0,0,0, 0,0,0,0, 0]]
    # },

}

solver = "CPM_ortools"
print("Solver:",solver)
for p in problems:
    print(f"problem: {p}")
    hidato(problems[p],solver)
    print()


solver = "CPM_minizinc"
print("Solver:",solver)
for p in problems:
    print(f"problem: {p}")
    hidato(problems[p],solver)
    print()
