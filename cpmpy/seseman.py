"""
Seseman convent problem in cpmpy.

Description of the problem:

n is the length of a border
There are (n-2)^2 "holes", i.e.
there are n^2 - (n-2)^2 variables to find out.

The simplest problem, n = 3 (n x n matrix)
which is represented by the following matrix:

 a b c 
 d   e 
 f g h 

Where the following constraints must hold:

  a + b + c = border_sum
  a + d + f = border_sum
  c + e + h = border_sum
  f + g + h = border_sum
  a + b + c + d + e + f = total_sum

For a (Swedish) discussion of this problem, see
'Sesemans matematiska klosterproblem samt lite Constraint Logic Programming'
http://www.hakank.org/webblogg/archives/001084.html
and
'Seseman's Convent Problem': http://www.hakank.org/seseman/seseman.cgi
(using Eclipse code)

It was also is commented in the (Swedish) blog post
'Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc'
http://www.hakank.org/webblogg/archives/001209.html
      

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def seseman():
    n = 8
    x = intvar(0,100,shape=n,name="x")
    a,b,c,d,e,f,g,h = x
    
    rowsum          = intvar(5,12,name="rowsum") # 9
    totalsum        = intvar(18,34,name="totalsum") # 24
    model = Model(
                   [
                   rowsum >= 0,
                   totalsum >= 0,
                   a+b+c+d+e+f+g+h == totalsum,
                   a+b+c == rowsum,
                   a+d+f == rowsum,
                   c+e+h == rowsum,
                   f+g+h == rowsum,
                   ]
        )

    def print_sol():
        print(a.value(), b.value(), c.value())
        print(d.value()," ", e.value())
        print(f.value(), g.value(), h.value())
        print("rowsum: ", rowsum.value(), " totalsum: ", totalsum.value())
        print()

    num_solutions = model.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)

seseman()

