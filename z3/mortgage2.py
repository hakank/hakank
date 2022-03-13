# 
# Mortgage problem in z3.
#
# From the lecture notes Modelling with Constraints
# www.cse.unsw.edu.au/~cs4418/2008/Lectures/Modelling2.ppt, page 56
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
import math
from z3 import *

def mortgage(time_limit,interest,principal):
    
    # s = SimpleSolver()
    # s = Solver()
    # s = SolverFor("QF_LIRA")
    s = Optimize()


    monthly_payment = Real("monthly_payment")
    # monthly_payment = Int("monthly_payment")    
    s.add(monthly_payment >= 0)
    
    b = Real("b") # balance at end
    s.add(b >= 0)
    
    # principal/current debt
    p = [Real(f"p[{i}]") for i in range(time_limit+1)]

    # t = time to end of loan    
    for t in range(1,time_limit+1):
        s.add(p[t] >= 0)
        s.add(p[t-1] == p[t] * (1.0 + interest/100.0) - monthly_payment)

    s.add(principal == p[time_limit])
    s.add(b == p[0])
    
    s.minimize(b)

    while s.check() == sat:
         mod = s.model()
         print("monthly_payment:", mod[monthly_payment].as_decimal(6), "b:", mod[b].as_decimal(6))
         print("p:", [mod[p[i]].as_decimal(6) for i in range(time_limit+1)])
         s.add(b < mod[b])

    principal = 100000

# 15 year mortgage (180 months), $100,000 principal, interest 1%/month
time_limit = 180 # timeframe
principal = 100000
interest = 1.0  # interest (%)
mortgage(time_limit,interest,principal)
