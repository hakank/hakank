"""
  Hanging weights puzzle in cpmpy.
  
  From 'Using LINQ to solve puzzles'
  http://blogs.msdn.com/lukeh/archive/2007/03/19/using-linq-to-solve-puzzles.aspx
  '''
  Here's a puzzle similar to the one in the puzzle hunt.  The diagram 
  below is a bunch of weights (A-M) hanging from a system of bars.  
  Each weight has an integer value between 1 and 13, and the goal is 
  to figure out what each weight must be for the the diagram below to 
  balance correctly as shown: 

                           |
                           |
               +--+--+--+--+--+--+--+
               |                    |
               |                    |
            +--+--+--+--+--+        |
            |     L        M        |
            |                       |
   +--+--+--+--+--+--+     +--+--+--+--+--+
   H              |  I     |  J        K  |
                  |        |              |
         +--+--+--+--+--+  |     +--+--+--+--+--+
         E              F  |     G              |
                           |                    |
               +--+--+--+--+--+  +--+--+--+--+--+--+
               A              B  C                 D

  The rules for this kind of puzzle are: 
  (1) The weights on either side of a given pivot point must be equal, 
      when weighted by the distance from the pivot, and 
  (2) a bar hanging beneath another contributes it's total weight as 
      through it were a single weight.  For instance, the bar on the bottom 
      right must have 5*C=D, and the one above it must have 3*G=2*(C+D).
  '''
  

  Here is a solution

                            |
                            |
                +--+--+--+--+--+--+--+
                |                    |
                |                    |
             +--+--+--+--+--+        |
             |     7        5        |
             |                       |
    +--+--+--+--+--+--+     +--+--+--+--+--+
   11              |  1     |  4        13 |
                   |        |              |
          +--+--+--+--+--+  |     +--+--+--+--+--+
          6              9  |     8              |
                            |                    |
                +--+--+--+--+--+  +--+--+--+--+--+--+
                3              12 2                 10


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *

def show(v):
    return str(v.value())

def hanging_weights():

    N = 13
    x = intvar(1,N,shape=N,name="x")
    a,b,c,d,e,f,g,h,i,j,k,l,m = x
    # total = intvar(1,n*n,name="total")

    model = Model([# total == sum(x),
                   AllDifferent(x),
                   4 * a == b, 
                   5 * c == d, 
                   3 * e == 2 * f, 
                   3 * g == 2 * (c + d), 
                   3 * (a + b) + 2 * j == k + 2 * (g + c + d), 
                   3 * h == 2 * (e + f) + 3 * i, 
                   (h + i + e + f) == l + 4 * m, 
                   4 * (l + m + h + i + e + f) == 3 * (j + k + g + a + b + c + d)  
                   ])

    def print_sol():
        print(x.value())
        print()
        # print(total.value())
        # This is copied from the MiniZinc model.
        print(
"                          |" + "\n" +
"                          |"+ "\n" +
"              +--+--+--+--+--+--+--+" + "\n" +
"              |                    |" + "\n" +
"              |                    |"+ "\n" +
"           +--+--+--+--+--+        |"+ "\n" +
"           |     "+show(l)+"        " +show(m)+"        |"+ "\n" +
"           |                       |"+ "\n" +
"  +--+--+--+--+--+--+     +--+--+--+--+--+"+ "\n" +
" "+show(h)+"              |  "+show(i)+"     |  "+show(j)+"        "+show(k)+" |"+ "\n" +
"                 |        |              |"+ "\n" +
"        +--+--+--+--+--+  |     +--+--+--+--+--+"+ "\n" +
"        "+show(e)+"              "+show(f)+"  |     "+show(g)+"              |"+ "\n" +
"                          |                    |"+ "\n" +
"              +--+--+--+--+--+  +--+--+--+--+--+--+"+ "\n" +
"              "+show(a)+"              "+show(b)+" "+show(c)+"                 "+show(d)+"\n")
        

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    ss.solveAll(display=print_sol)

hanging_weights()
