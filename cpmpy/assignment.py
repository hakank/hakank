"""
Assignment problem in cpmpy.

Some different assignment problems that use the
general assignment_model constraint.

See below for the sources of each problem.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

# Presentation for problem 3
def print_solution3(x,tasks,people):
    rows = len(tasks)
    cols = len(people)
    for i in range(rows):
        print(tasks[i], end="")
        for j in range(cols):
            if x[i][j] == 1:
                print(" with", people[j])


assignment_problems = {
    #
    # Winston 'Operations Research', Assignment Problems, page 393f
    # (generalized version with added test column)
    # 
    # hakank: I added the fifth column to make it more
    #         interesting
    "problem1" : {
      "cost" : [[14,  5, 8,  7, 15],
                [ 2, 12, 6,  5,  3],
                [ 7,  8, 3,  9,  7],
                [ 2,  4, 6, 10,  1]]
    },

    # 
    # Winston "Operations Research", page 398, swimming team example
    # (original version]
    # See http://www.hakank.org/minizinc/assignment2.mzn 
    # 
    "problem2" : {
       "cost" : [[54, 54, 51, 53], 
                 [51, 57, 52, 52],
                 [50, 53, 54, 56],
                 [56, 54, 55, 53]]
    },


    # 
    # Winston "Operations Research", page 398, swimming team example
    # The expanded version.
    # 
    "problem2_2": {
      "cost" : [[54, 54, 51, 53,   50,60,70,80,90,100], 
                [51, 57, 52, 52,   40,50,60,70,80, 90],
                [50, 53, 54, 56,   40,50,60,80,93, 69],
                [56, 54, 55, 53,   60,80,40,60,50,100]]
    },

    #
    # Winston "Operations Research", page 399
    # '''
    # Tom Cruise, Freddy Prinze Jr, Harrison Ford, and Matt LeBlanc
    # are marooned on a desert island with Jennifer Anniston,
    # Courtney Cos, Gwynneth Paltrow, and Julia Roberts.
    # The 'compatibility matrix' in Table 52 indicate how much happiness
    # each couple would experience if the spend all their time toghether.
    # The happiness earned by a couple is proportional to the fraction 
    # of time the spend toghether. 
    # ...
    # The optimal solution requires that that each person send all their
    # time with one person of the opposite sex, so this result is often
    # referred to as the Marriage Theorem.
    # '''
    # 
    # males:
    # 1 "Tom Cruise"
    # 2 "Freddie Prinz Jr"
    # 3 "Harrison Ford"
    # 4 "Mark LeBlanc"
    # 
    # females:
    # 1 "Jennifer Anniston"
    # 2 "Courtney Cox"
    # 3 "Gwynneth Paltrow"
    # 4 "Julia Roberts"
    # 
    # This is a maximization problem, and we define a special
    # method for presenting the solution.
    # The variable "tasks" for male is perhaps not completely appropriate.
    #
    "problem3" : {
      "opt" : "max",
      "cost" : [[7, 5, 8, 2],
                [7, 8, 9, 4],
                [3, 5, 7, 9],
                [5, 5, 6, 7]],
      "tasks": ["Tom Cruise","Freddie Prinz Jr","Harrison Ford","Mark LeBlanc"],
      "people" : ["Jennifer Anniston", "Courtney Cox","Gwynneth Paltrow","Julia Roberts"],
      "print_solution": print_solution3
      },
      

    # From
    #  "SAS OR 9.1 User's Guide Mathematical Programming"
    # '''
    # Consider assigning five programmers to five programming jobs. Each
    # programmer prefers specific programming job over others. [...] 
    # Suppose you ask each programmer to rank the jobs according to preference
    # (using 1 for the most preferred job and 5 for the least preffered job].
    # PROC ASSIGN maximizes the total preference of the group by minimizing the
    # sum of the preferences. 
    # 
    #    PROGRAMMER     JOB1 JOB2 JOB3 JOB4 JOB5
    #    PROGRAMMER1    4    1    3    5    2
    #              2    2    1    3    4    5
    #              3    3    2    4    1    5
    #              4    2    3    4    5    1
    #              5    4    2    3    1    5
    # 
    # '''
    "problem5" : {
      "cost": [[4, 1, 3, 5, 2],
               [2, 1, 3, 4, 5],
               [3, 2, 4, 1, 5],
               [2, 3, 4, 5, 1],
               [4, 2, 3, 1, 5]]
      },

    #
    # From GLPK:s example assign.mod:
    # '''
    # The assignment problem is one of the fundamental combinatorial
    # optimization problems.
    #
    # In its most general form, the problem is as follows:
    #
    # There are a number of agents and a number of tasks. Any agent can be
    # assigned to perform any task, incurring some cost that may vary
    # depending on the agent-task assignment. It is required to perform all
    # tasks by assigning exactly one agent to each task in such a way that
    # the total cost of the assignment is minimized.
    #
    # (From Wikipedia, the free encyclopedia.]
    # 
    # ...
    #
    # These data correspond to an example from [Christofides].
    # '''
    "problem6" : {
    "cost" : [[13, 21, 20, 12,  8, 26, 22, 11],
              [12, 36, 25, 41, 40, 11,  4,  8],
              [35, 32, 13, 36, 26, 21, 13, 37],
              [34, 54,  7,  8, 12, 22, 11, 40],
              [21,  6, 45, 18, 24, 34, 12, 48],
              [42, 19, 39, 15, 14, 16, 28, 46],
              [16, 34, 38,  3, 34, 40, 22, 24],
              [26, 20,  5, 17, 45, 31, 37, 43]]
    }
    
}

#
# Run an assignment problem.
#
def run_assignment_problem(problem):
    cost = problem["cost"]
    tasks = problem.get("tasks",None)
    people = problem.get("people",None)
    opt = problem.get("opt",None)
    print_solution = problem.get("print_solution",None)
    assignment_model(cost, tasks,people,print_solution,opt)


for p in sorted(assignment_problems):
    print(f"problem {p}")
    run_assignment_problem(assignment_problems[p])
    print()
