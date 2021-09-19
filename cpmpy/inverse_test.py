"""
Global constraint inverse (aka assigment) in cpmpy.


Picat> N=3,X=new_list(N),X::1..N,Y=new_list(N),Y::1..N,assignment(X,Y), solve_all([X,Y])=All,Len=All.len    
N = 3
X = [DV_019228_1..3,DV_019278_1..3,DV_0192c8_1..3]
Y = [DV_019398_1..3,DV_0193e8_1..3,DV_019438_1..3]
All = [[[1,2,3],[1,2,3]],[[1,3,2],[1,3,2]],[[2,1,3],[2,1,3]],[[2,3,1],[3,1,2]],[[3,1,2],[2,3,1]],[[3,2,1],[3,2,1]]]
Len = 6


x: [1 2 3 4] x_inv: [1 2 3 4]
x: [1 2 4 3] x_inv: [1 2 4 3]
x: [1 3 2 4] x_inv: [1 3 2 4]
x: [1 3 4 2] x_inv: [1 4 2 3]
x: [1 4 2 3] x_inv: [1 3 4 2]
x: [1 4 3 2] x_inv: [1 4 3 2]
x: [2 1 3 4] x_inv: [2 1 3 4]
x: [2 1 4 3] x_inv: [2 1 4 3]
x: [2 3 1 4] x_inv: [3 1 2 4]
x: [2 3 4 1] x_inv: [4 1 2 3]
x: [2 4 1 3] x_inv: [3 1 4 2]
x: [2 4 3 1] x_inv: [4 1 3 2]
x: [3 1 2 4] x_inv: [2 3 1 4]
x: [3 1 4 2] x_inv: [2 4 1 3]
x: [3 2 1 4] x_inv: [3 2 1 4]
x: [3 2 4 1] x_inv: [4 2 1 3]
x: [3 4 1 2] x_inv: [3 4 1 2]
x: [3 4 2 1] x_inv: [4 3 1 2]
x: [4 1 2 3] x_inv: [2 3 4 1]
x: [4 1 3 2] x_inv: [2 4 3 1]
x: [4 2 1 3] x_inv: [3 2 4 1]
x: [4 2 3 1] x_inv: [4 2 3 1]
x: [4 3 1 2] x_inv: [3 4 2 1]
x: [4 3 2 1] x_inv: [4 3 2 1]

[[1,2,3,4],[1,2,3,4]],
[[1,2,4,3],[1,2,4,3]],
[[1,3,2,4],[1,3,2,4]],
[[1,3,4,2],[1,4,2,3]],
[[1,4,2,3],[1,3,4,2]],
[[1,4,3,2],[1,4,3,2]],
[[2,1,3,4],[2,1,3,4]],
[[2,1,4,3],[2,1,4,3]],
[[2,3,1,4],[3,1,2,4]],
[[2,3,4,1],[4,1,2,3]],
[[2,4,1,3],[3,1,4,2]],
[[2,4,3,1],[4,1,3,2]],
[[3,1,2,4],[2,3,1,4]],
[[3,1,4,2],[2,4,1,3]],
[[3,2,1,4],[3,2,1,4]],
[[3,2,4,1],[4,2,1,3]],
[[3,4,1,2],[3,4,1,2]],
[[3,4,2,1],[4,3,1,2]],
[[4,1,2,3],[2,3,4,1]],
[[4,1,3,2],[2,4,3,1]],
[[4,2,1,3],[3,2,4,1]],
[[4,2,3,1],[4,2,3,1]],
[[4,3,1,2],[3,4,2,1]],
[[4,3,2,1],[4,3,2,1]]]

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def print_solution(a):
    print("x: ", a[0].value(), " inv: ", a[1].value())
    
def inverse_test(n=4):
    
    x = intvar(0,n-1,shape=n,name="x")
    x_inv  = intvar(0,n-1,shape=n,name="x_inv")
    model = Model(
        AllDifferent(x),
        AllDifferent(x_inv),
       
        inverse(x,x_inv),
        )

    # print(model)

    # ortools_wrapper(model,[x,x_inv],print_solution,0)
    ss = CPM_ortools(model)
    num_solutions = 0
    while ss.solve():
        num_solutions += 1
        print("x:",x.value()+1,"x_inv:",x_inv.value()+1,)
        get_different_solution(ss,flatten_lists([x,x_inv]))
    print("num_solutions:",num_solutions)

inverse_test()

