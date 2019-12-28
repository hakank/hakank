#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Bus driver scheduling problem (prob022 in CSPLib) in Z3
#
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/index.html
# 
# From 
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/spec.html
# """
# Specification
# Bus driver scheduling can be formulated as a set paritioning problem. 
# We propose 12 set partitioning problems derived from small bus driver 
# scheduling problems. These consist of a given set of tasks (pieces of 
# work) to cover and a large set of possible shifts, where each shift 
# covers a subset of the tasks and has an associated cost. We must select 
# a subset of possible shifts that covers each piece of work once and 
# only once: this is called a partition. Further,
# 
# In the driver scheduling (unlike air crew scheduling) the main aim is 
# to reduce the number of shifts used in the solution partition and the 
# total cost of the partition is secondary. To simplify the problem we have 
# made the cost of each shift the same. This means that the goal is to 
# minimise the number of shifts.
# 
# The problems come from four different bus companies: 
# Reading (r1 to r5a), 
# CentreWest Ealing area (c1, c1a, c2), 
# the former London Transport (t1 and t2). 
#
# The problems have differing regulations and features (e.g. urban and 
# short distance rural bus schedules can have very different features). Note 
# that r1 and r1a are the same problem, but have different numbers of 
# generated shifts. Similarly with the problems: c1, c1a and r5, r5a. 
# 
# Problems are presented in the same format as the set partitioning 
# examples in ORLIB. The first line gives the number of rows (pieces of work), 
# columns (shifts) and the minimum number of columns need for a partition. 
# Then each line after that corresponds to one column. It starts with 
# the cost (which is always 1 in our case) then the number of rows it 
# covers, followed by the rows it covers. 
# """
#
# The datafiles are in the directory
#    http://hakank.org/z3/bus_scheduling_csplib/problems/
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

#
# Read the data file
#
def read_problem(file):
  f = open(file, "r")
  num_rows,num_shifts,min_num_shifts = [int(t) for t in f.readline().replace('\n','').split(' ')]
  data = []
  for i in range(num_shifts):
      t = [int(t) for t in f.readline().rstrip().split(' ')]
      data.append([t[j] for j in range(2,len(t))])
  return [num_rows,num_shifts,min_num_shifts,data]



def bus_scheduling(problem):
    
    sol = SolverFor("LIA")
    # sol = Solver()

    # data
    num_work       = problem[0]
    num_shifts     = problem[1]
    min_num_shifts = problem[2]
    shifts         = problem[3]
    print("num_work:", num_work, "num_shifts:", num_shifts, "min_num_shifts:", min_num_shifts)
    
    # variables
    
    x = makeIntVector(sol,"x",num_shifts,0,1) # x[i] = 1 if this shift is selected
    tot_shifts = makeIntVar(sol,"tot_shifts", 0, min_num_shifts)

    # constraints
    sol.add(tot_shifts == sum(x))
    
    for j in range(num_work):
        sol.add(1 == Sum([x[i]*(Sum([If(k == j,1,0) for k in shifts[i]]) >= 1) for i in range(num_shifts)]))

    sol.add(tot_shifts >= min_num_shifts)

    print("solve")
    while sol.check() == sat:
        mod = sol.model()
        print("tot_shifts:", mod[tot_shifts])
        # print("x:", [mod[x[i]] for i in range(num_shifts)])
        selected_shifts = [i for i in range(num_shifts) if mod[x[i]] == 1]
        print("selected shifts:")
        ss = [0 for i in range(num_work)]
        for s in selected_shifts:
           print("shift ", s, ":", shifts[s])
           for i in shifts[s]:
               ss[i] = s
        # for w in range(num_work):
        #    print("work", w, "is covered in shift", ss[w])
        print()
        getLessSolution(sol,mod,tot_shifts)

# the problems in bus_scheduling_csplib/problems/
problems = [
     #name  size (bytes) order by size
    "t1",  #   1127
    "r1",  #  46974
    "r2",  #  53383
    "t2",  #  69564
    "r4",  #  71752
    "r5",  #  77707
    "r1a", #  80572
    "c1",  # 113966
    "c1a", # 228132
    "c2",  # 444719
    "r5a", # 496153
    "r3",  # 780452
    ]

p = "t1"
if __name__ == "__main__":
    if len(sys.argv) > 1:
        p= sys.argv[1]
    print("problem", p)
    path = "bus_scheduling_csplib/problems/%s"%p
    if not os.path.isfile(path):
        print("The file %s does not exists!" % path)
    else:
        problem = read_problem(path)
        bus_scheduling(problem)

