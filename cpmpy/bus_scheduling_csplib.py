"""
Bus driver scheduling problem (prob022 in CSPLib) in cpmpy.
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/index.html

From 
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/spec.html
'''
Specification
Bus driver scheduling can be formulated as a set paritioning problem. 
We propose 12 set partitioning problems derived from small bus driver 
scheduling problems. These consist of a given set of tasks (pieces of 
work) to cover and a large set of possible shifts, where each shift 
covers a subset of the tasks and has an associated cost. We must select 
a subset of possible shifts that covers each piece of work once and 
only once: this is called a partition. Further,

In the driver scheduling (unlike air crew scheduling) the main aim is 
to reduce the number of shifts used in the solution partition and the 
total cost of the partition is secondary. To simplify the problem we have 
made the cost of each shift the same. This means that the goal is to 
minimise the number of shifts.

The problems come from four different bus companies: 
Reading (r1 to r5a), 
CentreWest Ealing area (c1, c1a, c2), 
the former London Transport (t1 and t2). 

The problems have differing regulations and features (e.g. urban and 
short distance rural bus schedules can have very different features). Note 
that r1 and r1a are the same problem, but have different numbers of 
generated shifts. Similarly with the problems: c1, c1a and r5, r5a. 

Problems are presented in the same format as the set partitioning 
examples in ORLIB. The first line gives the number of rows (pieces of work), 
columns (shifts) and the minimum number of columns need for a partition. 
Then each line after that corresponds to one column. It starts with 
the cost (which is always 1 in our case) then the number of rows it 
covers, followed by the rows it covers. 
'''

The datafiles are in the directory
   http://hakank.org/cpmpy/bus_scheduling_csplib/problems/

Here's the walltimes (seconds) with a 60s timeout:

t1 : 0.012489291000000001
r1 : 1.4382032850000002
r2 : 1.864917573,
t2 : 2.6930278050000003,
r4 : 3.383277532,
r5 : 60,
r1a: 3.1755979450000003,
c1 : 5.129078195,
c1a: 11.481672312,
c2 : 60,
r5a: 60,
r3 : 25.730254748


The output for problem t1:

  num_work: 24 num_shifts: 77 min_num_shifts: 7
  tot_shifts: 7
  selected shifts:
  shift  1 : [11, 3, 4]
  shift  9 : [1, 2, 14, 15]
  shift  17 : [7, 18, 19, 20]
  shift  24 : [12, 13, 5, 6]
  shift  35 : [8, 9, 16, 17]
  shift  72 : [10, 22, 23]
  shift  76 : [21, 0]

  Num conflicts: 8
  NumBranches: 103
  WallTime: 0.012489291000000001



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import os
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


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


def bus_scheduling(problem,timeout=60):
    
    # data
    num_work       = problem[0]
    num_shifts     = problem[1]
    min_num_shifts = problem[2]
    shifts         = problem[3]
    print("num_work:", num_work, "num_shifts:", num_shifts, "min_num_shifts:", min_num_shifts)
    
    # variables
    
    x = boolvar(shape=num_shifts,name="x") # x[i] = 1 if this shift is selected
    tot_shifts = intvar(0, min_num_shifts,name="tot_shifts")

    # constraints
    model = Model(tot_shifts == sum(x))

    for j in range(num_work):
       model += [1 == sum([x[i]*(sum([k == j for k in shifts[i]]) >= 1) for i in range(num_shifts)])]

    model += (tot_shifts >= min_num_shifts)

    model.minimize(tot_shifts)
    print("before ss")
    ss = CPM_ortools(model)

    # ss.ort_solver.parameters.log_search_progress = True
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0
    if ss.solve(time_limit=timeout):
        print("tot_shifts:", tot_shifts.value())
        selected_shifts = [i for i in range(num_shifts) if x[i].value() == 1]
        print("selected shifts:")
        shifts_s = [0 for i in range(num_work)]
        for s in selected_shifts:
           print("shift ", s, ":", shifts[s])
           for i in shifts[s]:
               shifts_s[i] = s
        # for w in range(num_work):
        #    print("work", w, "is covered in shift", ss[w])
        print()
        print("Num conflicts:", ss.ort_solver.NumConflicts())
        print("NumBranches:", ss.ort_solver.NumBranches())
        walltime = ss.ort_solver.WallTime()
        print("WallTime:", walltime)
        print()
        return walltime
    else:
        print(f"Timeout: {timeout}s")
        return timeout



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

def benchmark(timeout=60):
  wall_times = {}
  print("Timeout:", timeout)
  for p in problems:
    print(f"problem {p}")
    path = "bus_scheduling_csplib/problems/%s"%p
    problem = read_problem(path)
    t = bus_scheduling(problem,timeout)
    wall_times[p] = t
    print(flush=True)

  print("Wall times:", wall_times)

# p = "t1"
# if len(sys.argv) > 1:
#   p= sys.argv[1]
# print("problem", p)
# path = "bus_scheduling_csplib/problems/%s"%p
# if not os.path.isfile(path):
#   print("The file %s does not exists!" % path)
# else:
#   problem = read_problem(path)
#   bus_scheduling(problem)

timeout = 60 # seconds
benchmark(timeout)
