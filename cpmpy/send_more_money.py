"""
SEND+MORE=MONEY in CPMpy

This is a different - and IMHO more natural - approach 
than the one in examples/send_more_money.py .

And in send_more_money2() there is a variant using scalar products.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

#
# "Plain algebraic" approach
#
def send_more_money():
    print("send_more_money")
    x = IntVar(0,9,shape=8)
    s,e,n,d,m,o,r,y = x
    constraints = [    (s*1000 + e*100 + n*10 + d) 
                     + (m*1000 + o*100 + r*10 + e) 
                    == (10000*m + 1000*o + 100*n + 10*e + y),
                  s > 0,m > 0,
                  AllDifferent(x)
                 ]
    
    model = Model(constraints)
    ss = CPM_ortools(model)
    stats = ss.solve()
    print(stats)
    print(x.value())

    print("status:", ss.status())
    
#
# Using scalar product
#
def send_more_money2():
    print("\nsend_more_money2 (prove unicity)")
    x = IntVar(0,9,shape=8)
    s,e,n,d,m,o,r,y = x
    send = IntVar(0,9999)
    more = IntVar(0,9999)
    money = IntVar(0,99999)        
    model = Model([s > 0,m > 0,
                   AllDifferent(x)
                   ]
                  )

    #
    # Some different ways to calculate the scalar product:
    #
    # base4 = base_array(4)
    # base5 = base_array(5)
    # model += [np.dot(base4,[s,e,n,d]) == send]
    # model += [np.dot(base4,[m,o,r,e]) == more]
    # model += [np.dot(base5,[m,o,n,e,y]) == money]
    
    # model += [sum(base4*[s,e,n,d]) == send]
    # model += [sum(base4*[m,o,r,e]) == more]
    # model += [sum(base5*[m,o,n,e,y]) == money]
    
    # model += [scalar_product(base4,[s,e,n,d]) == send]
    # model += [scalar_product(base4,[m,o,r,e]) == more]
    # model += [scalar_product(base5,[m,o,n,e,y]) == money]
    
    model += [scalar_product1([s,e,n,d]) == send]
    model += [scalar_product1([m,o,r,e]) == more]
    model += [scalar_product1([m,o,n,e,y]) == money]
    
    model += [send + more == money]

    # Prove that it's a unique solution.
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=x)    
    print(f"num_solutions: {num_solutions}")
    print(ss.status())


send_more_money()

send_more_money2()
