#!/usr/bin/python
"""
Bus scheduling in Numberjack.

Problem from Taha "Introduction to Operations Research", page 58.

This is a slightly more general model than Taha's.

Compare with the following models:
* MiniZinc: http://www.hakank.org/minizinc/bus_scheduling.mzn
* Comet   : http://www.hakank.org/comet/bus_schedule.co
* ECLiPSe : http://www.hakank.org/eclipse/bus_schedule.ecl
* Gecode  : http://www.hakank.org/gecode/bus_schedule.cpp
* Tailor/Essence'  : http://www.hakank.org/tailor/bus_schedule.eprime


This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *
# from NumberjackSolver import Solver
from Mistral import Solver
# from SCIP import Solver


def bus_schedule(libs, num_buses_check=0):

    time_slots = 6
    demands = [8, 10, 7, 12, 4, 4]
    max_num = sum(demands)
    x = VarArray(time_slots, 0, max_num)
    num_buses = Variable(0, max_num)
    
    model = Model(
        num_buses >= 0,
        num_buses == Sum(x),
        )

    # Show all solutions or minimise
    if num_buses_check > 0:
        print "show all solutions for", num_buses_check
        model.add(num_buses == num_buses_check)
    else:
        model.add(Minimise(num_buses))

    # Meet the demands for this and the next time slot
    for i in range(time_slots-1):
        model.add(x[i]+x[i+1] >= demands[i])

    # The demand "around the clock"
    model.add(x[time_slots-1] + x[0] == demands[time_slots-1])


    for library in libs:
        solver = model.load(library)
        print ''
        if solver.solve():
            print "num_buses: ", num_buses
            print "x:\n", x
            solver.printStatistics()
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            while num_buses_check > 0 and library == "Mistral" and solver.getNextSolution():
                print "num_buses: ", num_buses
                print "x:\n", x
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''
    # And return the value for usage later
    # if library == "SCIP":
    return num_buses.get_value()
        

num_buses_check = bus_schedule(['Mistral'])
bus_schedule(['Mistral'], num_buses_check)
