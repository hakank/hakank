"""
Moving furnitures (scheduling) problem in cpmpy.

Marriott & Stukey: 'Programming with constraints', page  112f

The model implements an experimental decomposition of the
global constraint cumulative.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def furniture_moving():

    #
    # data
    #
    n = 4
    duration = [30, 10, 15, 15]
    demand   = [ 3,  1,  3,  2]
    upper_limit = sum(duration)

    #
    # declare variables
    #
    start_times = intvar(0,upper_limit,shape=n,name="start_times")
    end_times   = intvar(0,upper_limit*2,shape=n,name="end_times")
    end_time    = intvar(0,upper_limit*2, name="end_time")

    # number of needed resources, to be minimized
    num_resources = intvar(0,4,name="num_resources")


    # z = num_resources*10 + end_time
    # z = end_time
    # z = num_resources
    z = sum(end_times) + num_resources * 10 
    # model = Model()
    model = Model(minimize=z)

    #
    # constraints
    #
    for i in range(n):
        model += [end_times[i] == start_times[i] + duration[i]]

    model += [end_time == max(end_times)]
    model += [my_cumulative(start_times, duration, demand, num_resources)]

    #
    # Some extra constraints to play with
    #
    # all tasks must end within an hour
    model += [end_time <= 60]
  
    # All tasks should start at time 0
    # for i in range(n):
    #    model += [start_times[i] == 0]

    # limitation of the number of people
    # model += [num_resources <= 3]

    def print_sol():
        print("num_resources:", num_resources.value())
        print("start_times  :", start_times.value())
        print("duration     :", [duration[i] for i in range(n)])
        print("end_times    :", end_times.value())
        print("end_time     :", end_time.value())
        print("z            :", z.value())      
        print()
        
 
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print()
    print("num_solutions:", num_solutions)
    

furniture_moving()
