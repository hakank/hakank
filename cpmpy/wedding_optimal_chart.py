"""
Finding an optimal wedding seating chart in cpmpy.

From 
Meghan L. Bellows and J. D. Luc Peterson
'Finding an optimal seating chart for a wedding'
http://www.improbable.com/news/2012/Optimal-seating-chart.pdf
http://www.improbable.com/2012/02/12/finding-an-optimal-seating-chart-for-a-wedding
'''
Every year, millions of brides (not to mention their mothers, future 
mothers-in-law, and occasionally grooms) struggle with one of the 
most daunting tasks during the wedding-planning process: the 
seating chart. The guest responses are in, banquet hall is booked, 
menu choices have been made. You think the hard parts are over, 
but you have yet to embark upon the biggest headache of them all. 
In order to make this process easier, we present a mathematical 
formulation that models the seating chart problem. This model can 
be solved to find the optimal arrangement of guests at tables. 
At the very least, it can provide a starting point and hopefully 
minimize stress and arguments… 
'''

Here is an optimal solution of problem3:

  problem problem3
  max_num_tables: 5 max_at_table: 10 min_knows_at_table: 3 num_guests: 17
  opt_type: maximize
  tables: [0 0 3 3 0 0 4 4 3 1 1 1 4 2 2 2 2]
  z: 273
  
  Table: 0
  ********************
  Deb        mother of the bride
  John       father of the bride
  Allan      grandfather of the bride
  Lois       wife of Allan (the grandfather of the bride)
  table_points: 104
  
  Table: 1
  ********************
  Mary Helen mother of the groom
  Lee        father of the groom
  Annika     sister of the groom
  table_points: 52
  
  Table: 2
  ********************
  Colin      brother of the groom
  Shirley    grandmother of the groom
  DeAnn      aunt of the groom
  Lori       aunt of the groom
  table_points: 6
  
  Table: 3
  ********************
  Martha     sister of the bride
  Travis     boyfriend of Martha (sister of the bride)
  Abby       cousin of the bride
  table_points: 61
  
  Table: 4
  ********************
  Jayne      aunt of the bride
  Brad       uncle of the bride
  Carl       brother of the groom
  table_points: 50
  
  
  ExitStatus.OPTIMAL (2.5733223020000002 seconds)
  Num conflicts: 66634
  NumBranches: 79283
  WallTime: 2.5733223020000002



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def wedding_optimal_chart(guest,names,names2,problem,opt_type="maximize"):

    max_num_tables = problem["max_num_tables"]
    max_at_table   = problem["max_at_table"]
    min_knows_at_table = problem["min_knows_at_table"]

    m = len(guest)

    n = max_num_tables
    a = max_at_table
    b = min_knows_at_table

    print("max_num_tables:",n,"max_at_table:",a,"min_knows_at_table:",b,"num_guests:",m)
    print("opt_type:",opt_type)
    
    # variables
    tables = intvar(0,n-1,shape=m,name="tables")
    z = intvar(0,10000,name="z")
        
    # constraints
    model = Model([z == sum(guests[j][k]*(tables[j]==tables[k])
                            for j in range(m) for k in range(j+1,m))])

    for i in range(n):
        # Minimum number of friends at table i
        model += [sum([(guests[j][k] > 0)*(tables[j] == i)*(tables[k] == i)
                       for j in range(m) for k in range(m) ]) >= b]

        # Min number of guests per table
        model += [sum([tables[j] == i for j in range(m)]) >= b]

        # Max number of guests per table
        model += [sum([tables[j] == i for j in range(m)]) <= a]

    # symmetry breaking
    model += [tables[0] == 0] # Deb sits at table 1

    if opt_type == "maximize":
        model.maximize(z)
    elif opt_type == "minimize":
        model.minimize(z)
    
    # print(model)

    def print_sol():
        print("tables:",tables.value())
        print("z:",z.value())
        print()
        for t in range(n):
            print("Table:",t)
            print("*"*20)
            people = []
            for i in range(m):
                if tables[i].value() == t:
                    print(names[i])
                    people.append(i)
            print("table_points:", sum([guests[p1][p2] for p1 in people for p2 in people if p1 < p2]))
            print()
        print()


    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.log_search_progress = True
    # ss.ort_solver.parameters.num_search_workers = 12 
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    


#
# Data
#

#  j   Guest         Relation
#  -------------------------------------
#  1   Deb           mother of the bride
#  2   John          father of the bride
#  3   Martha        sister of the bride
#  4   Travis        boyfriend of Martha
#  5   Allan         grandfather of the bride
#  6   Lois          wife of Allan (the grandfather of the bride)
#  7   Jayne         aunt of the bride
#  8   Brad          uncle of the bride
#  9   Abby          cousin of the bride
# 10   Mary Helen    mother of the groom
# 11   Lee           father of the groom
# 12   Annika        sister of the groom
# 13   Carl          brother of the groom
# 14   Colin         brother of the groom
# 15   Shirley       grandmother of the groom
# 16   DeAnn         aunt of the groom
# 17   Lori          aunt of the groom
#              Table 2: Guest List

# The "friend matrix". Higher value mean stronger bonds.
# Note the two clusters around the bride and groom.
guests = [[ 1,50, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [50, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1, 1,50, 1, 1, 1, 1,10, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1,50, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1, 1, 1, 1,50, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1, 1, 1,50, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1, 1, 1, 1, 1, 1,50, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1, 1, 1, 1, 1,50, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 1, 1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,50, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0,50, 1, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1]]


names = ["Deb        mother of the bride",
         "John       father of the bride",
         "Martha     sister of the bride",
         "Travis     boyfriend of Martha (sister of the bride)",
         "Allan      grandfather of the bride",
         "Lois       wife of Allan (the grandfather of the bride)",
         "Jayne      aunt of the bride",
         "Brad       uncle of the bride",
         "Abby       cousin of the bride",
         "Mary Helen mother of the groom",
         "Lee        father of the groom",
         "Annika     sister of the groom",
         "Carl       brother of the groom",
         "Colin      brother of the groom",
         "Shirley    grandmother of the groom",
         "DeAnn      aunt of the groom",
         "Lori       aunt of the groom"
         ]

names2 = ["Deb (B)",
          "John (B)",
          "Martha (B)",
          "Travis (B)",
          "Allan (B)",
          "Lois (B)",
          "Jayne (B)",
          "Brad (B)",
          "Abby (B)",
          "Mary Helen (G)",
          "Lee (G)",
          "Annika (G)",
          "Carl (G)",
          "Colin (G)",
          "Shirley (G)",
          "DeAnn (G)",
          "Lori (G)"
          ]

problems = {
    "problem1" : {
    "max_num_tables" : 5, # max number of tables
    "max_at_table" : 4,   # maximum number of guests a table can seat
    "min_knows_at_table" : 2 # minimum number of people each guest knows at their table
    },
    
    # Easier problem
    "problem2" : {
    "max_num_tables" : 2,
    "max_at_table" : 10,
    "min_knows_at_table" : 1
    },
    
    "problem3" : {
    "max_num_tables" : 5,
    "max_at_table" : 10,
    "min_knows_at_table" : 3
    }
}


opt_type = "maximize"
# opt_type = "minimize"
for p in problems:
    print(f"\nproblem {p}")
    wedding_optimal_chart(guests,names,names2,problems[p],opt_type)

