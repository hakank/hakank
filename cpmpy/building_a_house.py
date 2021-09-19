"""
Building a house, simple scheduling problem in cpmpy.

This model is adapted OPL model sched_intro.mod (examples).
'''
This is a basic problem that involves building a house. The masonry,
roofing, painting, etc.  must be scheduled. Some tasks must
necessarily take place before others, and these requirements are
expressed through precedence constraints.
'''

The OPL solution is
'''
 Masonry  : 0..35
 Carpentry: 35..50
 Plumbing : 35..75
 Ceiling  : 35..50
 Roofing  : 50..55
 Painting : 50..60
 Windows  : 55..60
 Facade   : 75..85
 Garden   : 75..80
 Moving   : 85..90
'''

With the extra objective (from the OPL model sched_time.mod) the result is

 masonry  : [20 -- 35 --> 55]
 carpentry: [75 -- 15 --> 90]
 plumbing : [55 -- 40 --> 95]
 ceiling  : [75 -- 15 --> 90]
 roofing  : [90 -- 5 --> 95]
 painting : [90 -- 10 --> 100]
 windows  : [95 -- 5 --> 100]
 facade   : [95 -- 10 --> 105]
 garden   : [95 -- 5 --> 100]
 moving   : [105 -- 5 --> 110]


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def prec(x, y, s, d):
  """
  prec(x, y, s, d)
  
  handle the precedences
  the task x must be finished before task y begin
  """
  return (s[x] + d[x] <= s[y])


def building_a_house(min_var="makespan"):
  print("min_var :",min_var)

  num_tasks = 10

  # for the precedences
  masonry,carpentry,plumbing,ceiling,roofing,painting,windows,facade,garden,moving = range(num_tasks)
  tasks = [masonry,carpentry,plumbing,ceiling,roofing,painting,windows,facade,garden,moving]
  tasks_s = ["masonry","carpentry","plumbing","ceiling","roofing","painting","windows","facade","garden","moving"]

  duration =  [35,15,40,15, 5,10, 5,10, 5, 5]
  height   =  [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] # resourse
  total_duration = sum(duration)

  # precendeces
  num_precedences = 14
  precedences = [[masonry,   carpentry], 
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

  start = intvar(0,total_duration,shape=num_tasks,name="start")
  end = intvar(0,total_duration,shape=num_tasks,name="end")

  limitx = intvar(1,3,name="limitx")
  makespan = intvar(0,total_duration,name="makespan")

  # the extra objective z (see above)
  z = intvar(0, 10000,name="z")

  # select which variable we should minimize: makespan or z
  if min_var == "makespan":
    min_val = makespan # (then we ignore the z part)
  else:
    min_val = z 

  model = Model(minimize=min_val)
  
  # constraints

  if min_val == z:
    model += (z ==
            400 * max([end[moving]- 100, 0])     +
            200 * max([25 - start[masonry], 0])   +
            300 * max([75 - start[carpentry], 0]) +
            100 * max([75 - start[ceiling], 0]))
  else:
    model += (z == 0)

  for t in range(num_tasks):
    model += (end[t] == start[t] + duration[t])
    
  # makespan is the end time of the last task
  model += (makespan == max(end))

  # precedences
  for p in range(num_precedences):
    model += (prec(precedences[p][0], precedences[p][1], start, duration))

  model += (my_cumulative(start, duration, height, limitx))

  
  ss = CPM_ortools(model)
  if ss.solve():
    print("min_val :", min_val.value())
    print("makespan:", makespan.value())
    print("z       :", z.value())    
    print("start   :", start.value())
    print("duration:", duration )
    print("height  :", height)       
    print("end     :", end.value())
    for t in range(num_tasks):
      print(f"{tasks_s[t]:10s}: {start[t].value():3d}..<{duration[t]:3d}>..{end[t].value():3d}")
    print()
      

building_a_house("makespan")

building_a_house("z")
