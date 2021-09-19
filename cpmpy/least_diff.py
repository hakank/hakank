"""
Least diff problem in cpmpyy.

The model solves the following problem:
 
 What is the smallest difference between two numbers X - Y
 if you must use all the digits (0..9) exactly once, i.e.
 minimize the difference ABCDE - FGHIJ.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

#
# Version 1
#
def least_diff1():

    x = IntVar(0,9, shape=10,name="x")
    a,b,c,d,e,f,g,h,i,j = x
    res                 = IntVar(0,200000,name="res")

    model = Model(minimize=res)
    model += [AllDifferent(x)]    
    model += [res == (a*10000 + b*1000 + c*100 + d*10 + e) - (f*10000 + g*1000 + h*100 + i*10 + j)]
    model += [a > 0,f > 0, res > 0]

    # model = Model(constraint,minimize=res)
    print("model:",model)
    ss = CPM_ortools(model)
    stats = ss.solve()
    print("res:", res.value())
    print("x:",x)
    print("x:", x.value())
    print("stats:",stats)
    print("status:",ss.status())    


# This is Tias' version
def least_diff2():
   # Vars
   x = IntVar(0,9, shape=10,name="x")
   a,b,c,d,e,f,g,h,i,j = x
   res                 = IntVar(0,200000,name="res")

   model = Model([
       AllDifferent(x),       
       res == (a*10000 + b*1000 + c*100 + d*10 + e) - (f*10000 + g*1000 + h*100 + i*10 + j),
       a > 0,
       f > 0,
       res > 0,
   ],minimize=res)
   ss = CPM_ortools(model)
   if ss.solve():
      print("res:",res.value())
      print("x:",x.value())
   print(model.status())

# Another approach
def least_diff3():
   x = IntVar(0,9, shape=10,name="x")
   a,b,c,d,e,f,g,h,i,j = x
   res                 = IntVar(0,200000,name="res")
    
   model = Model(minimize=res)
   model += [ AllDifferent(x) ]
   # model += [ res == sum([a,b,c,d,e] * np.flip(10**np.arange(5))) -
   #                   sum([f,g,h,i,j] * np.flip(10**np.arange(5)))]
   model += [ res == scalar_product1([a,b,c,d,e]) -
                     scalar_product1([f,g,h,i,j])]
   model += [ a > 0, f > 0, res > 0 ]

   print(model)
   ss = CPM_ortools(model)
   if ss.solve():
       print("RES =", res.value())       
       print("A,B,C,D,E=", "".join([str(v.value()) for v in [a,b,c,d,e]]))
       print("F,G,H,I,J=", "".join([str(v.value()) for v in [f,g,h,i,j]]))
   else:
       print("No solution found")
   print(model.status())


print("v1:")
least_diff1()

print("\nv2:")
least_diff2()

print("\nv3:")
least_diff3()

