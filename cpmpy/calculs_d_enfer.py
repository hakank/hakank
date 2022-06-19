"""
Calculs d'enfer puzzle in cpmpy.

Problem from Jianyang Zhou 'The Manual of NCL version 1.2', page 33
http://citeseer.ist.psu.edu/161721.html

The solution is the manual is:
'''
a = -16, b = -14, c = -13, d = -12, e = -10,
f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
x = 7, y = -2, z = -5.

max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
'''

Also, see the discussion of the Z model:
http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
(which shows the same solution as in the NCL book).


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def calculs_d_enfer():

    N = 26
    A = intvar(-100, 100,shape=N,name="A")
    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z = A
    
    A_abs = intvar(0,100,shape=N,name="A_abs")
    a_max = intvar(0,100,name="a_max")

    model = Model([
                    # [A_abs[I] == abs(A[I]) for I in range(N)],
                    A_abs == abs(A),
                    # [a_max >= A_abs[I] for I in range(N)],
                    a_max==max(A_abs),
                    AllDifferent(A),
                    z+e+r+o     == 0,
                    o+n+e       == 1,  
                    t+w+o       == 2,
                    t+h+r+e+e   == 3,
                    f+o+u+r     == 4,
                    f+i+v+e     == 5,
                    s+i+x       == 6,
                    s+e+v+e+n   == 7,
                    e+i+g+h+t   == 8,
                    n+i+n+e     == 9,
                    t+e+n       == 10,
                    e+l+e+v+e+n == 11,
                    t+w+e+l+f   == 12,
                    ],
                  minimize=a_max
                  )

    def print_sol():
        print("a_max:", a_max.value())        
        print("A:", A.value())
        print("A_abs:", A_abs.value())

    ss = CPM_ortools(model)
    ss.solveAll(display=print_sol)


calculs_d_enfer()


