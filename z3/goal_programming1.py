#  
# Goal programming in z3.
#
# Winston "Operations Research", page 191ff
# """
# The Leon Burnit Advertising Agency is trying to determine a
# TV advertisin schedule for Priceles Auto Company. Priceler has
# three goals:
# Goal 1: Its ads should be seen by at least 40 million high-income men (HIM)
# Goal 2:                                    60         low-income person (LIP) 
# Goal 3:                                    35         hight-incom women (HIW) 
#
# Leon Burnit can purchase two types of ad: those shown during football games
# and those shown during soap operas. At most, $6000000 can be spent on ads.
# The advertising costs and potential audience of a one-minute ad is:
#
#                  Million of 
#                    views  
# Ad           HIM   LIP   HIW  Cost
# Football     7      10    5    100000
# Soap-opera   3       5    4     60000
# """
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3 import *

def goal_programming1():
    
    # s = SimpleSolver()
    # s = SolverFor("QF_LIRA")
    s = Optimize()

    
    s1_penalty = 200.0
    s2_penalty = 100.0
    s3_penalty = 50.0
    

    # x1: football game
    # x2: soap operas
    x1,x2 = Reals("z1 z2")
    s.add(x1 >= 0, x2 >= 0)

    # penalties
    # s1_minus: penalty for x1 < limit
    # s2_minus 
    # s3_minus
    # s4_minus: penalty for exceeding budget
    s1_minus,s2_minus,s3_minus,s4_minus = Reals("s1_minus s2_minus s3_minus s4_minus")
    s.add(s1_minus >= 0, s2_minus >= 0, s3_minus >= 0, s4_minus >= 0)
    
    # s1_plus: penalty for x1 > limit
    # s2_plus 
    # s3_plus 
    # s4_plus 
    s1_plus,s2_plus,s3_plus,s4_plus = Reals("s1_plus s2_plus s3_plus s4_plus")
    s.add(s1_plus >= 0, s2_plus >= 0, s3_plus >= 0, s4_plus >= 0)

    # Minimize the penalties.
    # For s1..s3 it is a penalty to be below target.
    # for s4 (the budget) it is a penalty to exceed the budget 
    z = Real("z")
    s.add(z >= 0)
    s.add(z == s1_penalty*s1_minus + s2_penalty*s2_minus + s3_penalty*s3_minus + s4_plus)

    s.add(7.0*x1 +  3.0*x2 + s1_minus - s1_plus >=  40.0,   # HIM
          10.0*x1 +  5.0*x2 + s2_minus - s2_plus >=  60.0,  # LIP
          5.0*x1 +  4.0*x2 + s3_minus - s3_plus >=  35.0,   # HIW
          100.0*x1 + 60.0*x2 + s4_minus - s4_plus <= 600.0  # Budget constraint
          )

    s.minimize(z)
    
    while s.check() == sat:
        mod = s.model()
        print("z:", mod[z].as_decimal(6))
        print(f"x1:{mod[x1].as_decimal(6)} x2:{mod[x2].as_decimal(6)}")
        print(f"s1-:{mod[s1_minus].as_decimal(6)} s2-:{mod[s2_minus].as_decimal(6)} s3-:{mod[s3_minus].as_decimal(6)} s4-:{mod[s4_minus].as_decimal(6)}")
        print(f"s1+:{mod[s1_plus].as_decimal(6)} s2+:{mod[s2_plus].as_decimal(6)} s3+:{mod[s3_plus].as_decimal(6)} s4+:{mod[s4_plus].as_decimal(6)}")
        s.add(z < mod[z])


goal_programming1()

