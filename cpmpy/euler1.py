"""
Project Euler problem 1 in cpmpy.

http://projecteuler.net/index.php?section=problems&id=1
'''
If we list all the natural numbers below 10 that are multiples of 3 or 5, 
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
'''

Using Constraint Modeling for this problem is probably
a tad overkill...

This model (using ipython):
'''
In [1]: %time run euler1.py                                                                                  
z: 233168
status: ExitStatus.OPTIMAL (0.006237117 seconds)

CPU times: user 1.2 s, sys: 1.34 s, total: 2.53 s
Wall time: 197 ms
'''

Compare with this ipython oneliner.
'''
% time sum([i for i in range(1000) if (i %3 == 0 or i %5 == 0)])                                      
CPU times: user 66 µs, sys: 12 µs, total: 78 µs
Wall time: 80.3 µs
Out[1]: 233168
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def euler1():

  n = 1000
  x = boolvar(shape=n,name="x")
  z = intvar(0,sum(range(n)),name="z")

  model = Model([x[0] == 0,
                 [x[i] == ((i % 3==0) | (i % 5==0)) for i in range(1,n)],
                 z==sum([i*x[i] for i in range(n)])
                 ])

  ss = CPM_ortools(model)
  if ss.solve():
    print("z:", z.value())
    print("status:", ss.status())

euler1()
