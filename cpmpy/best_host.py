"""
Best host (PuzzlOr problem) in cpmpy.

From The PuzzlOr, February | Volume 38 | Number 1
http://www.informs.org/ORMS-Today/Public-Articles/February-Volume-38-Number-1/THE-PUZZLOR
'''
By John Toczek

Hosting a dinner party requires several skills to pull off a successful 
evening. One of your duties, aside from preparing dinner and selecting 
the drinks, is to make sure your guests enjoy themselves.

Figure 1 shows a dinner table with six seats for your guests. Some 
guests, however, do not get along with each other. If two guests who 
do not get along are seated next to each other, it will create conflict 
at dinner. As host, you must arrange the guests in a seating order 
that minimizes conflict.

Andrew will only sit next to Dave and Frank; 
Betty will only sit next to Cara and Erica; 
Cara will only sit next to Betty and Frank; 
Dave will only sit next to Andrew and Erica; 
Erica will only sit next to Betty and Dave; 
Frank will only sit next to Andrew and Cara. 

[
  Figure 1 shows the following arrangement:

              Andrew
       Frank         Betty
       Erica         Cara
              Dave

]

In the example seating arrangement above, there are three conflicts 
(Andrew and Betty, Cara and Dave, Erica and Frank).

Question:

What seating arrangement will minimize the conflict at dinner?
'''

Answer: There are 12 possible solutions with no conflict at all.

By placing Andrew at position 1 (as symmetry breaking) there are 
2 possible solutions, where the 2nd solution is the mirror of the 1st.

1) Andrew Frank Cara Betty Erica Dave

                Andrew
         Dave           Frank
         Erica          Cara
                Betty

2) Andrew Dave Erica Betty Cara Frank
     
                 Andrew 
          Frank         Dave
          Cara          Erica
                Betty 



"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def member_of2(x, val):
    """
    member_of(x, val)

    Ensures that the value `val` is in the array `x`.
    """
    print(f"member_of2({x},{val})")
    constraints = [sum([v == val for v in x]) > 0]    
    return constraints



def best_host():
    
    model = Model()

    #
    # data
    #
    n = 6

    Andrew = 0
    Betty  = 1
    Cara   = 2
    Dave   = 3
    Erica  = 4
    Frank  = 5

    name_str = ["Andrew", "Betty", "Cara", "Dave", "Erica", "Frank"]

    prefs = cpm_array([
        [Dave, Frank],   # Andrew
        [Cara, Erica],   # Betty
        [Betty, Frank],  # Cara
        [Andrew, Erica], # Dave 
        [Betty, Dave],   # Erica
        [Andrew, Cara]   # Frank
        ])

    # declare variables
    x = intvar(0,n-1,shape=n,name="x")

    #
    # constraints
    #
    model += (AllDifferent(x))

    # symmetry breaking
    model += (x[0] == Andrew)

    for i in range(n):
        # This don't work:
        # TypeError: object of type 'Element' has no len()
        # model += [member_of(prefs[x[i]], x[(i-1) % n])]
        # model += [member_of(prefs[x[i]], x[(i+1) % n])]

        # It works if we expand the array:
        model += [member_of([prefs[x[i],j] for j in range(2)], x[(i-1) % n])]        
        model += [member_of([prefs[x[i],j] for j in range(2)], x[(i+1) % n])]

        # Another approach that works
        # model += [sum([prefs[x[i],j] == x[(i-1) % n] for j in range(2)])>0]
        # model += [sum([prefs[x[i],j] == x[(i+1) % n] for j in range(2)])>0]                  

    def print_sol():
        print(x.value())
        print(" ".join([name_str[x[i].value()] for i in range(n)]))
        print()
        
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)  


best_host()
