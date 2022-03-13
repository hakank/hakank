#
# Warehouse scheduling in z3.
#
# From "Problem Solving for the 21st Century - Efficient Solvers for Satisfiability Modulo Theories"
# by Clark Barrett, Daniel Kroening and Tom Melham
# https://www.lms.ac.uk/sites/default/files/SMT-KT-report-screen.pdf
# """
# Consider the following scheduling problem, adapted from [24]. Suppose two employees on duty
# are always needed to run a certain warehouse. The warehouse operates 24/7 and each day is
# divided into three shifts: day, afternoon, and night. There are nine employees available
# to cover the shifts, and we want to come up with a schedule for these employees. We can model
# this as an SMT problem as follows.
#
# Let S be a 9 × 7 matrix of integers so that S (i, j) represents which shift the i-th employee
# is working on the j-th day of the week. We encode each employee’s shifts as follows:
#
#    day = 1, afternoon = 10, night = 100, not working = 0.
#
# To represent the fact that every entry in S has to take one of these values, we
# write 63 separate constraints:
#
# S (1, 1) = 0 OR S (1, 1) = 1 OR S (1, 1) = 10 OR S (1, 1) = 100
# S (1, 2) = 0 OR S (1, 2) = 1 OR S (1, 2) = 10 OR S (1, 2) = 100
# ...
# S (9, 7) = 0 OR S (9, 7) = 1 OR S (9, 7) = 10 OR S (9, 7) = 100.
# 
# Using the shift encoding we have chosen, we can easily represent the fact that we need two
# workers for every shift by imposing seven summation constraints:
#
# S (1, 1) + S (2, 1) + S (3, 1) + S (4, 1) + S (5, 1) + S (6, 1) + S (7, 1) + S (8, 1) + S (9, 1) = 222
# S (1, 2) + S (2, 2) + S (3, 2) + S (4, 2) + S (5, 2) + S (6, 2) + S (7, 2) + S (8, 2) + S (9, 2) = 222
# ...
# S (1, 7) + S (2, 7) + S (3, 7) + S (4, 7) + S (5, 7) + S (6, 7) + S (7, 7) + S (8, 7) + S (9, 7) = 222.
# 
# This works because the only way to add nine shift numbers together to get 222 is if
# two of them are 100, two are 10, two are 1, and the rest are 0.
#
# A modern SMT solver can find a solution to all these constraints—producing a viable
# employee schedule to run the warehouse—in less than a second. Additional constraints can easily
# be added to represent minimum and maximum shift lengths, minimum and maximum number of days
# worked in a row, and illegal shift sequences (e.g. no one should work a day shift immediately
# after a night shift)
# """
#
# I added the following extra constraints:
# - no one should work a day shift immediately after a night or afternoon shift
# - min/max limits of the shifts per employee per week
# - max number of days worked in a row, and we assume a
#   rolling schedule for the employees
#   Note: There is no solution with max number of days in a row = 2,
#         we need at least 3.
# - objective: make the schedule as equal as possible.
#   Finding a sat solution is fast, but finding the optimal is slow.
#
# Here is one optimal solution:
#
# z: 62
# total_points: [13, 11, 13, 11, 11, 15, 13, 11, 14]
# Emp    0 1 2 3 4 5 6 7 8
# ________________________
# Day 0: - a n d - a n - d 
# Day 1: a n - d a - - d n 
# Day 2: n a - n a d - d - 
# Day 3: - - d - n a d n a 
# Day 4: d d a a - n - - n 
# Day 5: n d - a d - n a - 
# Day 6: - - n - d n a a d 
#

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3_utils_hakank import *

def warehouse_scheduling(num_employees=9,num_days=7,req_per_shift=2,max_days_in_row=3,use_opt=False):

    # s = SimpleSolver()
    # s = Solver()
    # s = SolverFor("LIA")
    s = SolverFor("ALL")
    # s = SolverFor("QF_FD")
    # s = SolverFor("QF_AUFLIA")
    # s = SolverFor("AUFLIA")        
    # s = Optimize() # too slow

    # s = Then(Tactic("simplify"),Tactic("smt")).solver()

    # Encode the shifts
    day_shift = 1
    afternoon = 10
    night = 100
    not_working = 0

    total_req = req_per_shift*(day_shift + afternoon + night)

    # Minimum/maximum number of shifts per employee per week
    min_shifts = {day_shift: 1, afternoon: 1, night: 1, not_working: 1}
    max_shifts = {day_shift: 2, afternoon: 2, night: 2, not_working: num_days}    

    # For the presentation
    h = {day_shift: "d", afternoon: "a", night: "n", not_working: "-"}

    # Decision variables
    shift = [ [Int(f"shift[{day},{emp}]") for emp in range(num_employees) ] for day in range(num_days)  ]
    for day in range(num_days):
        for emp in range(num_employees):
            s.add(Or(shift[day][emp] == not_working,
                     shift[day][emp] == day_shift,
                     shift[day][emp] == afternoon,
                     shift[day][emp] == night))

    # Required shifts per day
    for day in range(num_days):
        s.add(total_req == Sum([shift[day][emp] for emp in range(num_employees)  ]))

    # Extra: no one should work a day shift immediately after a night or afternoon shift
    for emp in range(num_employees):
        for day in range(1,num_days):
            s.add(If(shift[day-1][emp] == night, Or(shift[day][emp] == afternoon, shift[day][emp] == not_working), True))
            s.add(If(shift[day-1][emp] == afternoon, shift[day][emp] != day_shift, True))            

    # Extra: min/max limits of the shifts per employee per week
    for emp in range(num_employees):
        for sh in [day_shift,afternoon,night,not_working]:
            t = [If(shift[day][emp]==sh,1,0) for day in range(num_days)]
            s.add(Sum(t) >= min_shifts[sh])
            s.add(Sum(t) <= max_shifts[sh])

    # Extra: Max number of days worked in a row
    for emp in range(num_employees):
        # We assume a rolling schedule (not the modulo for the day)
        for day in range(num_days):
            # print("day:", [d % num_days for d in range(day,day+max_days_in_row+1)])
            s.add(Sum([If(shift[d % num_days][emp] != not_working,1,0) for d in range(day,day+max_days_in_row+1)]) <= max_days_in_row)


    #
    # Extra: make the schedule as equal as possible.
    # Note: One way would be the encoding of the shifts but that
    #       give too much weight to night shifts. 
    # And we cannot use a Python dict for this.
    # point_weights = {day_shift:1,afternoon:2,night:5,not_working:0}
    #
    # Function to the rescue!
    point_weights = Function("point_weights",IntSort(), IntSort())
    s.add(point_weights(day_shift)   == 1,
          point_weights(afternoon)   == 2,
          point_weights(night)       == 5,
          point_weights(not_working) == 0)
    total_points = [Int(f"total_points[{emp}]") for emp in range(num_employees)]
    for emp in range(num_employees):
        s.add(total_points[emp] == Sum([point_weights(shift[day][emp]) for day in range(num_days)]))

    # Total differences between employees (to minimize if use_opt)
    z = Int("z")
    # s.add(z >= 0)
    s.add(z == Sum([Abs(total_points[emp1] - total_points[emp2]) for emp1 in range(num_employees)
                    for emp2 in range(emp1+1,num_employees)]))

    # s.minimize(z)

    while s.check() == sat:
        mod = s.model()
        shift_val = [ [mod.eval(shift[day][emp]).as_long() for emp in range(num_employees) ] for day in range(num_days) ]
        print("z:", mod[z])
        print("total_points:", [mod[total_points[emp]].as_long() for emp in range(num_employees)])        
        print("Emp   "," ".join([str(emp) for emp in range(num_employees)]))
        print(24*"_")
        for day in range(num_days):
            print("Day",day,end=": ")
            for emp in range(num_employees):
                print(h[shift_val[day][emp]], end=" ")
            print()
        print()
                
        # getDifferentSolution(s,mod,[shift[day][emp] for day in range(num_days) for emp in range(num_employees)])
        # getDifferentSolution(s,mod,total_points)
        if use_opt:
            s.add(z < mod[z])


num_employees = 9
num_days = 7
req_per_shift = 2
max_days_in_row = 3
use_opt = False # True # False
warehouse_scheduling(num_employees,num_days,req_per_shift,max_days_in_row,use_opt)
