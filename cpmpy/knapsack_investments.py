"""
Knapsack (investment) problem in cpmpy.

From the Swedish book

Lundgren, Rönnqvist, Värbrand 'Optimeringslära' [Optimization Theory], page 393ff.

A company shall invest in some building projects with the following
limits:

 - budget of 225 Mkr (Million Swedish kronor)
 - 28 persons available
 - maximum 9 projects can be selected
 - some project may not be selected together with other projects, and some
   projects must be selected together with other.

(I've kept the Swedish object names.)

No.  Object   Value(kkr) Budget(Mkr) Personell  Not with  Requires
1  Ishall      600        35            5        10        -
2  Sporthall   400        34            3        -         -
3  Hotell      100        26            4        -         15
4  Restaurang  150        12            2        -         15
5  Kontor A     80        10            2        6         -
6  Kontor B    120        18            2        5         -
7  Skola       200        32            4        -         -
8  Dagis       220        11            1        -         7
9  Lager        90        10            1        -         -
10 Simhall     380        22            5        1         -
11 Hyreshus    290        27            3        15        -
12 Bilverkstad 130        18            2        -         -
13 Tennishall   80        16            2        -         2
14 Idrottsanl. 270        29            4        -         2
15 Båthamn     280        22            3        11        -

Solution (page 395): 
The following project is selected
  1,2,4,6,7,8,12,14,15
and optimal value is 2370kkr.

This model uses a more general model than the book's model.

The solution is the same as the book (well, we must check,
mustn't we? :-)

x = [1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1]
     1  2     4     6  7  8          12    14 15
total_values = 2370
total_projects = 9
total_persons = 26
total_budget = 211

Question: Is there another solution with total_values = 2370?
Answer: No, that's the unique solution.
 
Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def knapsack_investments(total_values_val=0):

    num_projects = 15 # number of projects to select from
    max_budget = 225 # budget limit 
    max_projects = 9 # max number of projects to select
    max_persons = 28 # persons available
    # the values and budgets of each project
    values = [600,400,100,150, 80,120,200,220, 90,380,290,130, 80,270,280]
    budgets = [35,34,26,12,10,18,32,11,10,22,27,18,16,29,22]

    # project i cannot be selected with project j
    num_not_with = 6
    not_with = [ 
        [1, 10],
        [5, 6],
        [6, 5],
        [10, 1],
        [11, 15],
        [15, 11]
        ]
    
    # project i requires project j 
    num_requires = 5
    requires = [
        [3, 15],
        [4, 15],
        [8, 7],
        [13, 2],
        [14, 2]
        ]
    
    personell = [5,3,4,2,2,2,4,1,1,5,3,2,2,4,3]

    #
    # decision variable
    #

    # what project to select
    x = boolvar(shape=num_projects,name="x")

    total_persons = intvar(0,max_persons,name="total_persons")
    total_budget  = intvar(0,max_budget, name="total_budget")
    total_projects = intvar(0,max_projects,name="total_project")

    # the objective to maximize
    total_values  = intvar(0,sum(values),name="total_values")

    model = Model(maximize=total_values)

    # solve maximize total_values;
    model += ([total_persons  == sum(x*personell),
               total_budget   == sum(x*budgets),
               total_projects == sum(x),
               total_values   == sum(x*values),
              
               # resource limits:
               total_budget <= max_budget,
               total_persons <= max_persons,
               total_projects <= max_projects,
               ])

    #
    # special requirements, using standard integer programming "tricks"
    #
    # projects that require other projects
    for i in range(num_requires):
        model += (x[requires[i][0]-1] - x[requires[i][1]-1] <= 0)

    # projects excluding other projects
    for i in range(num_not_with):
        model += (x[not_with[i][0]-1] + x[not_with[i][1]-1] <= 1)


    def print_sol():
        print("x:", [i+1 for i in range(num_projects) if x[i].value() == 1])
        print("total_values: ", total_values.value())
        print("total_projects: ", total_projects.value())
        print("total_persons: ", total_persons.value())
        print("total_budget: ", total_budget.value())
        print()
        
    ss = CPM_ortools(model)
    ss.solve()
    total_values_opt = total_values.value()
    print("total_values_opt:", total_values_opt)
    
    print("All optimal solutions:")
    ss += (total_values==total_values_opt)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)

knapsack_investments()

