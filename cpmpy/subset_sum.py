"""
Subset sum problem in cpmpy.

From Katta G. Murty: 'Optimization Models for Decision Making', page 340
http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
'''
Example 7.8.1

A bank van had several bags of coins, each containing either
16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
street, thieves stole some bags. A total of 100 coins were lost.
It is required to find how many bags were stolen.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *




def subset_sum(values, total):
    n = len(values)
    x = intvar(0, n,shape=n)
    ss = intvar(0, n)

    model = Model(
                ss == sum(x),
                total == sum(x*values)
        )

    return model, x, ss

def subset_sum_model(coins, total):

    model, x, ss = subset_sum(coins, total)

    def print_sol():
        print("ss:",ss.value())
        print("x:",x.value())

    model.solveAll(display=print_sol)


coins = [16, 17, 23, 24, 39, 40]
total = 100
subset_sum_model(coins, total)
