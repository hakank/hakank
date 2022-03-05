#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Building a house, simple scheduling problem in Z3
#
# This model is adapted OPL model sched_intro.mod (examples).
# """
# This is a basic problem that involves building a house. The masonry,
# roofing, painting, etc.  must be scheduled. Some tasks must
# necessarily take place before others, and these requirements are
# expressed through precedence constraints.
# """
#
# The OPL solution is
#  """
#  Masonry  : 0..35
#  Carpentry: 35..50
#  Plumbing : 35..75
#  Ceiling  : 35..50
#  Roofing  : 50..55
#  Painting : 50..60
#  Windows  : 55..60
#  Facade   : 75..85
#  Garden   : 75..80
#  Moving   : 85..90
#  """
# 
# With the extra objective (from the OPL model sched_time.mod) the result is
#
#  masonry  : [20 -- 35 --> 55]
#  carpentry: [75 -- 15 --> 90]
#  plumbing : [55 -- 40 --> 95]
#  ceiling  : [75 -- 15 --> 90]
#  roofing  : [90 -- 5 --> 95]
#  painting : [90 -- 10 --> 100]
#  windows  : [95 -- 5 --> 100]
#  facade   : [95 -- 10 --> 105]
#  garden   : [95 -- 5 --> 100]
#  moving   : [105 -- 5 --> 110]
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


# handle the precedences
#  the task x must be finished before task y begin
def prec(sol, x, y, s, d):
   sol.add(s[x] + d[x] <= s[y])


sol = SolverFor("LIA")


# data

num_tasks = 10

# for the precedences
masonry,carpentry,plumbing,ceiling,roofing,painting,windows,facade,garden,moving = range(num_tasks)
tasks = [masonry,carpentry,plumbing,ceiling,roofing,painting,windows,facade,garden,moving]
tasks_s = ["masonry","carpentry","plumbing","ceiling","roofing","painting","windows","facade","garden","moving"]

duration =  [35,15,40,15, 5,10, 5,10, 5, 5];
height   =  [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
total_duration = sum(duration)

# precendeces
num_precedences = 14;
precedences = [
    [masonry,   carpentry], 
    [masonry,   plumbing], 
    [masonry,   ceiling],
    [carpentry, roofing],
    [ceiling,   painting],
    [roofing,   windows],
    [roofing,   facade],
    [plumbing,  facade],
    [roofing,   garden],
    [plumbing,  garden],
    [windows,   moving],
    [facade,    moving],
    [garden,    moving],
    [painting,  moving]
]


# variables

start = makeIntVector(sol,"start",num_tasks, 0, total_duration)
end = makeIntVector(sol,"end",num_tasks, 0, total_duration)

limitx = makeIntVar(sol,"limitx",1,3)
makespan = makeIntVar(sol,"makespan", 0,total_duration)

# the extra objective z (see above)
z = makeIntVar(sol,"z", 0, 10000)

# select which variable we should minimize: makespan or z

min_val = makespan # (then we ignore the z part)
# min_val = z 


# constraints
# This takes a long time to calculate
# print("before cumulative")
cumulative(sol, start, duration, height, limitx, 0, total_duration)
# print("after cumulative")

if min_val == z:
    sol.add(z ==
            400 * maximum2(sol,[end[moving]- 100, 0])     +
            200 * maximum2(sol,[25 - start[masonry], 0])   +
            300 * maximum2(sol,[75 - start[carpentry], 0]) +
            100 * maximum2(sol,[75 - start[ceiling], 0]))
else:
    sol.add(z == 0)

for t in range(num_tasks):
    sol.add(end[t] == start[t] + duration[t])
# makespan is the end time of the last task
maximum(sol, makespan, end)

# precedences
for p in range(num_precedences):
    prec(sol,precedences[p][0], precedences[p][1], start, duration)



# minimize makespan;
while sol.check() == sat:
    mod = sol.model()
    print("makespan:", mod[makespan])
    if min_val == z:
        print("z:", mod[z])
    print("start:", [mod[start[t]] for t in range(num_tasks)])
    print("end  :", [mod[end[t]] for t in range(num_tasks)])
    for i in range(num_tasks):
        print("%-10s: %3i..(%3i)..%3i" % (tasks_s[i], mod[start[i]].as_long(), duration[i], mod[end[i]].as_long()))
    print()
    getLessSolution(sol,mod,min_val)




