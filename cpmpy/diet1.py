"""
Diet problem in cpmpy.

Standard Operations Research example.


Minimize the cost for the products:
Type of                        Calories   Chocolate    Sugar    Fat
Food                                      (ounces)     (ounces) (ounces)
Chocolate Cake (1 slice)       400           3            2      2
Chocolate ice cream (1 scoop)  200           2            2      4
Cola (1 bottle)                150           0            4      1
Pineapple cheesecake (1 piece) 500           0            4      5


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def diet1():

    n = 4
    price  = [ 50, 20, 30, 80] # in cents
    limits = [500,  6, 10,  8] # requirements for each nutrition type

    # nutritions for each product
    calories  = [400, 200, 150, 500]
    chocolate = [  3,   2,   0,   0]
    sugar     = [  2,   2,   4,   4]
    fat       = [  2,   4,   1,   5]
    
    x         = intvar(0, 10000,shape=n)
    cost      = intvar(0, 1000,name="cost")
    model = Model(
        [
        sum(x*calories)  >= limits[0],
        sum(x*chocolate) >= limits[1],
        sum(x*sugar)     >= limits[2],
        sum(x*fat)       >= limits[3],
        cost == sum(x*price),
        ],
        minimize=cost
        )

    def print_sol():
        print("x:",x.value())
        print("cost:",cost.value())
        print()

    model.solveAll(display=print_sol)

diet1()
