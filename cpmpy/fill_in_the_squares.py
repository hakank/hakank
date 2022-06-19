"""
Fill-in the squares problem (Brainjammer) in cpmpy.

This problem is from the ZDC system, available from 
http://www.bracil.net/CSP/cacp/cacpdemo.html , in the
file 
    Brainjammer.txt 
from 2003-01-26, which states:
'''
Only Solution is:
    1     2       3       4       5
====================================================
A    7      11      2       17      1
B   13      19      23      22      3
C   9       20      24      14      12
D   16      21      25      18      10
E   4       8       15      6       5

  22mins55secs of CPU time to find first solution
  50mins42secs of CPU time with duplicate induced variables removed?
  Maybe this has something to do with the variable ordering...as this might change
  as a result of removing duplicate induced variables.
  1hr:34 mins of CPU time to find a single solution and determine no other solutions
  exist.

  Statistics for finding the first solution:
  (with duplicate induced nodes removed)
  CPU seconds:                4880.63 (On a Pentium Pro 200Mhz, VC++)
  Node count:                 4036162
  Induced node count: 1849214
  Backtracks:                 5885311

'''

Notes: 
- On my dual core 3.2 Mhz (Linux) it takes about 0.3 seconds (runtime) to solve 
  this problem (0.058s solution time), but the comparison is really not fair
  considering the  difference in machines.
- The only references to this problem I've found are the following pages:
   http://discuss.fogcreek.com/techInterview/default.asp?cmd=show&ixPost=2787
   http://notdarkandstormy.blogspot.com/2005/05/funky-logic-problem.html
   and especially 
   http://perplexus.info/show.php?pid=2683
   which has a lot of comments about manually solving the problem.

I've yet to know the original source.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def fill_in_the_squares():

    model = Model()

    n = 5
    nn = n*n

    Primes = [2,3,5,7,11,13,17,19,23]
    Primes2 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
    Squares = [1,4,9,16,25]

    
    a = intvar(1,nn,shape=n,name="a")
    b = intvar(1,nn,shape=n,name="b")
    c = intvar(1,nn,shape=n,name="c")
    d = intvar(1,nn,shape=n,name="d")
    e = intvar(1,nn,shape=n,name="e")

    ALL = intvar(1,nn,shape=nn,name="ALL") # all numbers

    min_sum = 21
    max_sum = 25
    min_max_sum = sum([i for i in range(min_sum,max_sum+1)])
    a_sum = intvar(1,min_max_sum,name="a_sum")
    b_sum = intvar(1,min_max_sum,name="b_sum")
    c_sum = intvar(1,min_max_sum,name="c_sum")
    d_sum = intvar(1,min_max_sum,name="d_sum")
    e_sum = intvar(1,min_max_sum,name="e_sum")
    
    model += (a_sum == sum(a))
    model += (b_sum == sum(b))
    model += (c_sum == sum(c))
    model += (d_sum == sum(d))
    model += (e_sum == sum(e))

    #
    # Note: The constraint comments are 1-based, but Python is not.
    #
    
    # Each number from 1-25, used only once
    for i in range(n):
        model += ([
        ALL[i]     == a[i],
        ALL[i+n]   == b[i],
        ALL[i+2*n] == c[i],
        ALL[i+3*n] == d[i],
        ALL[i+4*n] == e[i],
        ])
  
    model+= (AllDifferent(ALL))

    # 1.Sum of each column is odd
    for i in range(n):
        model += ((a[i] + b[i] + c[i] + d[i] + e[i]) % 2 == 1)

  
    # 2.Sum of each row, except C is even
    model += ([
      a_sum % 2 == 0,
      b_sum % 2 == 0,
      c_sum % 2 == 1,
      d_sum % 2 == 0,
      e_sum % 2 == 0
      ])

  
    # 3.Sum of row A is not greater than the sum of any other row
    model += ([
      a_sum <= b_sum,
      a_sum <= c_sum,
      a_sum <= d_sum,
      a_sum <= e_sum,
      ])
  
    # 4.The sum of diagonal A1 to E5 is greater than the sum of
    #   diagonal E1 to A5
    model += (a[0] + b[1] + c[2] + d[3] + e[4]  > e[0] + d[1] + c[2] + b[3] + a[4])
  
    # 5.(A4 + B4) is greater than (C4+D4+E4)
    model += (a[3] + b[3] > c[3] + d[3] + e[3])
  
    # 6. A1 + B1 = D1 + E1
    model += (a[0] + b[0] == d[0] + e[0])
         
    # 7. A1 > E1
    model += (a[0] > e[0])
  
    # 8. A1, A3 and B1 are primes
    model += ([
             member_of(Primes,a[0]),
             member_of(Primes,a[2]),
             member_of(Primes,b[0])
             ])
    # 9.(A3 + E3) is a prime number
    model += (member_of(Primes,a[2]+e[2]))

  
    # 10. A5,D1,D3 and E1 are squares
    model += ([
      member_of(Squares,a[4]),
      member_of(Squares,d[0]),
      member_of(Squares,d[2]),
      member_of(Squares,e[0]),       
      ])

    # 11. B2, C2, and D2 are ascending consecutive numbers
    model += ([
      b[1] + 1 == c[1],
      c[1] + 1 == d[1],
      ])
  
    # 12. B3, C3, and D3 are ascending consecutive numbers
    model += ([
        b[2] + 1 == c[2],
        c[2] + 1 == d[2]
        ])
  
    # 13. B5 + D5 = A5 + C5
    model += (b[4] + d[4] == a[4] + c[4])
  
    # 14. (c1)^2 + (c5)^2 = (e3)^2
    model += (c[0]*c[0] + c[4]*c[4] == e[2]*e[2])
  
    # 15. C5 is a two-digit number
    model += (c[4] > 9)
  
    # 16. D5 is a multiple of E5
    model += (d[4] % e[4] == 0)
  
    # 17. E1 + E3 = E2 + E4 + E5
    model += (e[0] + e[2] == e[1] + e[3] + e[4])

    def print_sol():
        print("a:",a.value())
        print("b:",b.value())
        print("c:",c.value())
        print("d:",d.value())
        print("e:",e.value())        
        print()
        
    
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)
    print(ss.status())

fill_in_the_squares()
