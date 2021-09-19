"""
Decomposition of diffn constraint in cpmpy.



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import os,random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

  
def diffn_test():
    
    # n = 14 # size of main square
    # a = [1,1,1,1,2,3,3,3,5,6,6,8] # Sizes

    n = 6
    a = [1,1,1,1,1,2,3,3,3]

    size = len(a)

    x = intvar(1,n,shape=size,name="x")
    y = intvar(1,n,shape=size,name="y")    

    # constraints
    model = Model([diffn(x,y,a,a),
                   # cumulative(start_times, duration, demand, num_resources)
                   my_cumulative(x,a,a,size),
                   my_cumulative(y,a,a,size)
                   ])

    for i in range(size):
        model += (x[i] + a[i] <= n+1)
        model += (y[i] + a[i] <= n+1)

    ortools_wrapper(model,[x,y])

    

diffn_test()
