"""
Magic hexagon in cpmpy.

Prob023: Magic Hexagon
http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/
'''
A magic hexagon consists of the number 1 to 19 arranged in a hexagonal pattern:

   A,B,C
  D,E,F,G
 H,I,J,K,L
  M,N,O,P
   Q,R,S

We have a constraint that all diagonals sum to 38. That is,
   A+B+C = D+E+F+G = ... = Q+R+S = 38, A+D+H = B+E+I+M = ... = L+P+S = 38,
   C+G+L = B+F+K+P = ... = H+M+Q = 38.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def magic_hexagon():

    N = 19
    LD = intvar(1, N, shape=N,name="LD")
    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s = LD

    model = Model (
        AllDifferent(LD),
        a + b + c ==  38,
        d + e + f + g ==  38,
        h + i + j + k + l ==  38, 
        m + n + o + p ==  38, 
        q + r + s ==  38, 
        a + d + h ==  38, 
        b + e + i + m ==  38, 
        c + f + j + n + q ==  38, 
        g + k + o + r ==  38, 
        l + p + s ==  38, 
        c + g + l ==  38, 
        b + f + k + p ==  38, 
        a + e + j + o + s ==  38, 
        d + i + n + r ==  38, 
        h + m + q ==  38, 
        a < c,
        a < h,
        a < l,
        a < q,
        a < s,
        c < h
        )

    model.solveAll(display=LD)


magic_hexagon()


