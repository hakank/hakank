"""
Test of the cumulative constraint in cpmpy.

The (decomposition of) cumulative constraint is defined in cpmpy_hakank.py
as my_cumulative.

Also, see the following models that use cumulative:
 - furniture_movining.py




Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import os,random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


#
# Example from Global constraints catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Ccumulative.html
# """
# cumulative(TASKS,LIMIT)
# 
# Purpose
# 
# Cumulative scheduling constraint or scheduling under resource constraints. 
# Consider a set T of tasks described by the TASKS collection. The cumulative 
# constraint enforces that at each point in time, the cumulated height of 
# the set of tasks that overlap that point, does not exceed a given limit. 
# It also imposes for each task of T the constraint origin+duration=end.
# 
# Example
#    (
#    <
#     origin-1  duration-3  end-4   height-1,
#     origin-2  duration-9  end-11  height-2,
#     origin-3  duration-10 end-13  height-1,
#     origin-6  duration-6  end-12  height-1,
#     origin-7  duration-2  end-9   height-3
#     >,8
#     )
#
# Figure 4.71.1 [see the web page] shows the cumulated profile associated with 
# the example. To each task of the cumulative constraint corresponds a set of 
# rectangles coloured with the same colour: the sum of the lengths of the 
# rectangles corresponds to the duration of the task, while the height of the 
# rectangles (i.e., all the rectangles associated with a task have the same 
# height) corresponds to the resource consumption of the task. The cumulative 
# constraint holds since at each point in time we don't have a cumulated 
# resource consumption strictly greater than the upper limit 8 enforced by 
# the last argument of the cumulative constraint.
# """
#
def test1():
  
  duration = [3,9,10, 6,2]
  demand =  [1,2,1,1,3]
  max_num_resources = 10
  max_end_time = 20
  
  cumulative_test(duration,demand,max_num_resources,max_end_time)


# Example from SICStus doc:
# http://www.sics.se/sicstus/docs/4.0.1/html/sicstus/Cumulative-Scheduling.html
# """
# This example is a very small scheduling problem. We consider seven 
# tasks where each task has a fixed duration and a fixed amount of used resource:
#
# Task 	Duration  Resource
# t1    16        2
# t2     6        9
# t3    13        3
# t4     7        7
# t5     5       10
# t6    18        1
# t7     4       11
#
# The goal is to find a schedule that minimizes the completion time for the 
# schedule while not exceeding the capacity 13 of the resource. The resource 
# constraint is succinctly captured by a cumulative/2 constraint. Branch-and-bound 
# search is used to find the minimal completion time. 
# """
#
def test2():
  duration = [16,6,13,7,5,18,4]
  demand = [2,9,3,7,10,1,11]
  max_num_resources = 13
  max_end_time = 30

  cumulative_test(duration,demand,max_num_resources,max_end_time)


#
# Run the specific instance. 
#
def cumulative_test(duration,demand,max_num_resources,max_end_time):

  model = Model()

  n = len(duration)

  # declare variables

  # note: we define start time at 0
  start_times = intvar(0,max_end_time,shape=n,name="start_times")
  end_times = intvar(0,max_end_time+max(duration),shape=n,name="end_times")  
  end_time = intvar(0,max_end_time+max(duration),name="end_time")

  # number of needed resources, perhaps to be minimized
  num_resources = intvar(0,max_num_resources,name="num_resources")

  #
  # constraints
  #
  for i in range(n):
    model += [end_times[i] == start_times[i] + duration[i]]
    
  model += [end_time == max(end_times)]
  model += [my_cumulative(start_times, duration, demand, num_resources)]

  # Combined objective
  model.minimize(end_time*10 + num_resources*100)

  ss = CPM_ortools(model)

  num_solutions = 0
  if ss.solve() is not False:
    num_solutions += 1
    print("num_resources:", num_resources.value())
    print("demand       :", demand)
    print("start_times  :", start_times.value())
    print("duration     :", duration)
    print("end_times    :", end_times.value())
    print("end_time     :", end_time.value())
    print()
    
   
print("test1:")
test1()
print("\ntest2:")
test2()
