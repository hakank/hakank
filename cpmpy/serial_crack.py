"""
Serial crack in cpmpy.

From https://rolandsako.wordpress.com/2016/02/17/playing-with-z3-hacking-the-serial-check/
'''
First, let's break out the algorithm into simple constraints.
The serial must :

 -  Be in the format XXXX-XXXX-XXXX-XXXX (to make thing easier to follow,
    let's call them groups of numbers, there are 4 groups right?)
 -   Where X is an int between [0..9]
        except for fourth_group where it must be in [3..8]
 -   fourth_group will be used as a cheksum
        its sum must be equal to the average of the first 3 groups
        its average must be equal to the sum of first_group
 -   sum of first_group must be different to sum of second_group
 -   first_group and fourth_group can't have a similar value at the same index
 -   second_group and third_group can't have a similar int at the same index

[
    0 1 2 3  - 0 1 2 3 - 0 1 2 3 - 0 1 2 3
    X X X X    X X X X   X X X X   X X X X
    first      second    third     fourth
    group      group     group     group
]
You could brute-force it, but you want to be sure you make it
before 4:00am and brute-forcing is cheating.

The output should be as follow :
[a__1 = 0,
a__0 = 4,
c__1 = 8,
b__3 = 0,
a__2 = 0,
a__3 = 0,
d__3 = 7,
b__0 = 7,
d__1 = 3,
d__0 = 3,
c__2 = 7,
b__2 = 6,
c__0 = 8,
c__3 = 1,
b__1 = 7,
d__2 = 3]
'''

However, this is NOT a unique solution! It has a huge number of solutions,
something that the author don't address. However, it actually is
a nice feature of a serial number generator.



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def serial_crack():

    n = 4
    first  = intvar(0,9,shape=n,name="first")
    second = intvar(0,9,shape=n,name="second")
    third  = intvar(0,9,shape=n,name="third")
    fourth = intvar(3,8,shape=n,name="fourth")    

    sum_first  = intvar(0,50,name="sum_first")
    sum_second = intvar(0,50,name="sum_second")
    sum_third  = intvar(0,50,name="sum_third")
    sum_fourth = intvar(0,50,name="sum_fourth")
        
    avg_sums = intvar(0,50,name="avg_sums")

    model = Model([sum_first  == sum(first),
                   sum_second == sum(second),
                   sum_third  == sum(third),
                   sum_fourth == sum(fourth),

                   # average of groups, excluding the 4th (checksum)
                   3*avg_sums == (sum_first + sum_second + sum_third), 
                   
                   # * fourth_group will be used as a cheksum
                   #   - its sum must be equal to the average of the first 3 groups
                   sum_fourth == avg_sums,

                   #   - its average must be equal to the sum of first_group
                   sum_first*4 == sum_fourth,

                   # sum of first_group must be different to sum of second_group                   
                   sum_first != sum_second,

                   # first_group and fourth_group can't have a similar value at the same index             
                   sum([first[i] == fourth[i] for i in range(n)]) == 0,

                   # second_group and third_group can't have a similar int at the same index               
                   sum([second[i] == third[i] for i in range(n)]) == 0,
                   ])

    def print_sol():
        for g in [first.value(), second.value(), third.value(), fourth.value()]:          
            print("{}{}{}{}".format(g[0], g[1], g[2], g[3]),end=" ")
        print()

    ss = CPM_ortools(model)
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0
    num_solutions = model.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)


serial_crack()
