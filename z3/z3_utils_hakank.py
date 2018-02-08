#!/usr/bin/python -u
# -*- coding: latin-1 -*-
#
# Utilities and (decompositions of) global constraints in Z3.
#
# Here I have collected some useful (or perhaps not that useful) methods for z3py.
# These where added mostly for simplifying the porting of my "traditional"
# constraint programming models.
#
# Convenience wrappers for creating variables and their domains etc
#
# - makeIntVar(sol,name,min_val, max_val)
# - makeIntVarVals(sol,name,vals)
# - makeIntVars(sol,name,size, min_val, max_val)
# - makeIntVector(sol,name,min_val, max_val)
# - makeIntArray(sol,name,min_val, max_val)
# - makeIntArrayVector(sol,name,min_val, max_val)
#
# - makeRealVar(sol,name,min_val, max_val)
# - makeRealVars(sol,name,size, min_val, max_val)
# - makeRealVector(sol,name,min_val, max_val)
#
# - getDifferentSolution(sol,mod,*params)
# - getLessSolution(sol,mod,z)
# - getGreaterSolution(sol,mod,z)
#
# - evalArray(mod,a)
# - print_grid(sol,mod,x,num_rows,num_cols)

#
# Global constraints (decompositions) in Z3
#
# - all_different(sol,x)
# - all_different_except_0(sol,x)
# - increasing(sol,x)
# - decreasing(sol,x)
# - count(sol, value, x, n)
# - global_cardinality_count(sol, values, x, gcc)
# - at_most(sol, v,x,max)
# - at_least(sol, v,x,min)
# - scalar_product(sol, a,x,product)
# - product == scalar_product2(sol, a,x)
# - circuit(sol,z,path,n)  See circuit.py
# - inverse(sol,f,invf,n)
# - maximum(sol,max,x)
# - minimum(sol,min,x)
# - abs(x)
# - toNum
# - subset_sum(sol, values, total)
# - allowed_assignments(sol,t,allowed)  aka table, table_in
# - member_of(sol, e, v)
# - no_overlap(sol, s1, d1, s2, d2)
# - sliding_sum(sol, low, up, seq, x)
# - bin_packing(sol,capacity, bins, weights)
#
# 
# TODO
# element not needed! z = x[y] works for Array/3 (but not for Int or IntVector)
# lex_(le|lt|ge|gt)(sol,x,y)  : array x is lexicographic (equal or) less/greater than array y
# nvalue
# matrix_element: stable_marriage.py needs that!
# cumulative See furniture_moving.py for a start..
# diffn?
# regular???
# subcircuit???
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3 import *

#
# Utils to create Int, IntVector, Array etc
# as well as the fiddling of evaluation and ensuring new solutions.
#

# creates Int() with a domain
def makeIntVar(sol,name,min_val, max_val):
    v = Int(name)
    sol.add(v >= min_val, v <= max_val)
    return v

def makeIntVarVals(sol,name,vals):
    v = Int(name)
    sol.add(Or([v == i for i in vals]))
    return v

#  creates [ Int() for i in range(size)] with a domains
def makeIntVars(sol,name,size, min_val, max_val):
    a = [Int("%s_%i" % (name,i)) for i in range(size)]
    [sol.add(a[i] >= min_val, a[i] <= max_val) for i in range(size)]
    return a

# creates an IntVector with a domain
def makeIntVector(sol,name, size, min_val, max_val):
    v = IntVector(name,size)
    [sol.add(v[i] >= min_val, v[i] <= max_val) for i in range(size)]
    return v

# creates an Array with a domain
def makeIntArray(sol,name, size, min_val, max_val):
    a = Array(name,IntSort(),IntSort())
    [sol.add(a[i] >= min_val, a[i] <= max_val) for i in range(size)]
    return a

# creates an Array with a domain, and returns an array
def makeIntArrayVector(sol,name, size, min_val, max_val):
    a = Array(name,IntSort(),IntSort())
    [ sol.add(a[i] >= min_val, a[i] <= max_val) for i in range(size)]
    return [a[i] for i in range(size)]


def makeRealVar(sol,name,min_val, max_val):
    v = Real(name)
    sol.add(v >= min_val, v <= max_val)
    return v

#  creates [ Real() for i in range(size)] with a domains
def makeRealVars(sol,name,size, min_val, max_val):
    a = [Real("%s_%i" % (name,i)) for i in range(size)]
    [sol.add(a[i] >= min_val, a[i] <= max_val) for i in range(size)]
    return a

# creates an IntVector with a domain
def makeRealVector(sol,name, size, min_val, max_val):
    v = RealVector(name,size)
    [sol.add(v[i] >= min_val, v[i] <= max_val) for i in range(size)]
    return v



#
# When using
#    while sol.check() == sat:
# one must add some differences to get new solutions.
#
# Usage:
#   addDifferentSolution(sol,mod,x,y,z,...)
# where x,y,z,.. are arrays.
# 
# Note: For the optimization problems, one should use either
#   addLessSolution(sol,mod,z)
# for minimization problems
# or
#   addGreaterSolution(sol,mod,z)
# for maximization problems.
#
def getDifferentSolution(sol,mod, *params):
    for t in params:
        sol.add(Or([t[i] != mod.eval(t[i]) for i in range(len(t))]))

# special case for a matrix; requires number of rows and columns
def getDifferentSolutionMatrix(sol,mod, x, rows, cols):
    sol.add(Or([x[i,j] != mod.eval(x[i,j]) for i in range(rows) for j in range(cols)]))

# ensure that we get a solution with a less value of z
def getLessSolution(sol,mod, z):
    sol.add(z < mod.eval(z))

# ensure that we get a solution with a greater value of z
def getGreaterSolution(sol,mod, z):
    sol.add(z > mod.eval(z))

# evalArray(mod,a)
# return an evaluated array
def evalArray(mod,a):
    return [mod.eval(a[i]) for i in range(len(a))]

# print_grid(sol,mod,x,num_rows,num_cols)
# prints an (unformatted) grid/matrix
def print_grid(mod,x,rows,cols):
    for i in range(rows):
        for j in range(cols):
            print mod.eval(x[(i,j)]),
        print
    print


#
# Decompositions of global constraints
#

# all_different_except_0/2
def all_different_except_0(sol,x):
    for i in range(len(x)):
        for j in range(i):
            sol.add( Implies(Or(x[i] != 0, x[j] != 0), x[j] != x[i]  ))

# all_different/2
# (but one should probably use Distinct/1 instead...)
def all_different(sol,x):
    for i in range(len(x)):
        for j in range(i):
            sol.add( x[i] != x[j])

# increasing_strict/2
def increasing_strict(sol,x):
    for i in range(len(x)-1):
        sol.add(x[i] < x[i+1])

# increasing/2
def increasing(sol,x):
    for i in range(len(x)-1):
        sol.add(x[i] <= x[i+1])

# decreasing_strict/2
def decreasing_strict(sol,x):
    for i in range(len(x)-1):
        sol.add(x[i] > x[i+1])

# decreasing/2
def decreasing(sol,x):
    for i in range(len(x)-1):
        sol.add(x[i] >= x[i+1])

# count/4:
# * if n is Int(): count the number of value in x
# * if n is fixed: ensure that the number of value in the x array is exactly n
# * if both value and n are Int()'s: count one/all value(s)
def count(sol,value,x,n):
    sol.add(n == Sum([If(x[i] == value, 1,0) for i in range(len(x))]))

# count/3
# same as count/4 but returns the sum value 
def count2(sol,value,x):
    return Sum([If(x[i] == value, 1,0) for i in range(len(x))])


# global_cardinality_count/4
# * gcc[v] containts the occurrences of the value of values[v] in array x
#   (it's a generalization of count/4)
def global_cardinality_count(sol,values,x,gcc):
    for v in range(len(values)):
        count(sol,values[v],x,gcc[v])

# at_most/4
# * there are at most max occurrences of value v in x
def at_most(sol,v,x,max):
    c = Int("c")
    sol.add(c>=0, c <= len(x))
    count(sol,v,x,c)
    sol.add(c <= max)

# at_least/4
# * there are at least max occurrences of value v in x
def at_least(sol,v,x,min):
    c = Int("c")
    sol.add(c>=0, c <= len(x))
    count(sol,v,x,c)
    sol.add(c >= min)

# scalar_product(sol,a,x,product)
# ensures that a[*]*x[*] == product
def scalar_product(sol,a,x,product):
    sol.add(product == Sum([a[i]*x[i] for i in range(len(x))]))

# product == scalar_product2(sol,a,x)
# ensures that Sum([a[i]*x[i] ... ]  == product
def scalar_product2(sol,a,x):
    return Sum([a[i]*x[i] for i in range(len(x))])

#
# constraint(sol,x,path,n)
# find a (Hamiltonian) circuit of x and its path path
# n is the size of x and path
def circuit(sol, x, z, n):
    # z = Array('z',IntSort(), IntSort())
    # for i in range(n):
    #     sol.add(z[i] >= 1, z[i] <= n)

    #
    # The main constraint is that Z[I] must not be 1 
    # until I = N, and for I = N it must be 1.
    #

    sol.add(Distinct([x[i] for i in range(n)])),
    sol.add(Distinct([z[i] for i in range(n)])),
    
    # first element of x[0] == z[0]
    sol.add(x[0] == z[0])
   
    # The last element in z must be 1 (back to original spot)
    sol.add(z[n-1] == 1)

    # Get the orbit for Z.
    for i in range(1,n):
        # I'm very happy that this element works! Z3 is cool. :-)
        sol.add(x[z[i-1]] == z[i])


# inverse(..f, invf, ..)
# ensures that each value in f is the position in invf, and vice versa
# Note that we are 0-based so the domain of both arrays are 0..n-1!
#
# See inverse.py
#
def inverse(sol, f, invf, n):
    for i in range(n):
        for j in range(n):
            sol.add((j == f[i]) == (i == invf[j]))

# max is the maximum value of x
def maximum(sol, max, x):
  sol.add(Or([max == x[i] for i in range(len(x))])) # max is an element in x)
  for i in range(len(x)):
    sol.add(max >= x[i]) # and it's the greatest


# min is the minimum value of x
def minimum(sol, min, x):
  sol.add(Or([min == x[i] for i in range(len(x))])) # min is an element in x)
  for i in range(len(x)):
    sol.add(min <= x[i]) # and it's the smallest

# absolute value of x
def Abs(x):
    return If(x >= 0,x,-x)

# converts a number (s) <-> an array of integers (t) in the specific base.
# See toNum.py
def toNum(sol, t, s, base):
  tlen = len(t)
  sol.add(s == Sum([(base ** (tlen - i - 1)) * t[i] for i in range(tlen)]))

# subset_sum(sol,values,total)
# total is the sum of the values of the select elements in values
# returns array of the selected entries and the sum of the selected values
def subset_sum(sol, values, total):
  n = len(values)
  x = [makeIntVar(sol,"x_%i"%i,0,n) for i in range(n)]
  ss = makeIntVar(sol,"ss", 0, n)

  sol.add(ss == Sum([x[i] for i in range(n)]))
  sol.add(total == scalar_product2(sol,x, values))

  return x, ss

# allowed_assignments(sol,t,allowed):
# a.k.a. table, table_in etc
# ensure that the tuple (list) t is in the list allowed
# (of allowed assignments)
def allowed_assignments(sol,t,allowed):
    len_allowed = len(allowed)
    t_len = len(t)
    sol.add(
        Or([ And([t[a] == allowed[k][a] for a in range(t_len)])
             for k in range(len_allowed)]
           ))

# ensure that element e is one of v
def member_of(sol, e, v):
    sol.add(Or([e == i for i in v]))


# No overlapping of tasks s1 and s2
def no_overlap(sol, s1, d1, s2, d2):
  sol.add(Or(s1 + d1 <= s2, s2 + d2 <= s1))

#
# sliding_sum(sol,low,up,seq,x)
# ensures that the sum of all subsequences in x of length seq
# are between low and up
# low, up, and seq must be fixed integers
#
def sliding_sum(sol, low, up, seq, x):
  vlen = len(x)
  for i in range(vlen-seq+1):
    s = makeIntVar(sol, "s_%i"%i,low,up)    
    sol.add(s == Sum([x[j] for j in range(i,i+seq)]))


#
# bin_packing
#
# Note: capacity (and bins) might be IntVar but weights must be an int vector
#
def bin_packing(sol,capacity, bins, weights):
  n = len(bins)
  for b in range(n):
    sol.add(Sum([ weights[j]*If(bins[j] == b,1,0) for j in range(n)] ) <= capacity)



# Some experiments
if __name__ == "__main__":    
    sol = Solver()
    n = 5
    # x = IntVector("x",n)
    # for i in range(n):
    #     sol.add(x[i]>=0, x[i] <= n)
    x = makeIntVector(sol,"x",n,0,n)
    # sol.add(Distinct(x))
    # all_different_except_0(sol,x)
    # all_different(sol,x)

    increasing(sol,x)
    # increasing_strict(sol,x)
    # decreasing(sol,x)
    # decreasing_strict(sol,x)

    # exactly twp 0s
    # count(sol,0,x,2)

    # count the number of 0's
    c = Int("c")
    sol.add(c >= 0, c <= n)
    count(sol,0,x,c) # simple example

    # Here we also let the value free (i.e. not just checking 0)
    # So we count the number of all values 1..n
    # v = Int(v)
    # sol.add(v >= 0, v <= n)
    # count(sol,v,x,c)

    gcc = IntVector("gcc",n+1)
    for i in range(n):
        sol.add(gcc[i] >= 0, gcc[i] <= n+1)
    # for i in [i for i in range(n)]:
    #     nn = Int("nn")
    #     # sol.add(nn>=0, nn<=n+1)
    #     count(sol,i,x,gcc[i])
    #     # sol.add(gcc[i] == nn)
    global_cardinality_count(sol,[i for i in range(0,n+1)], x, gcc)

    # enfore that we should have 2 0s
    # sol.add(gcc[0] == 1)

    at_most(sol,2,x,2)
    at_least(sol,2,x,2)

    num_solutions = 0
    print sol.check()
    while sol.check() == sat:
        num_solutions = num_solutions + 1
        mod = sol.model()
        ss = [mod.eval(x[i]) for i in range(n)]
        cc = mod.eval(c)
        # vv = m.eval(v)
        gccs = ([mod.eval(gcc[i]) for i in range(n)])
        # print ss, " #0s: ", mod.eval(cc), " v:", m.eval(v)
        print ss, " #0s: ", mod.eval(cc), " gcc:", gccs
        sol.add(
            Or(
            Or([x[i] != ss[i] for i in range(n)]),
            cc != c 
            , Or([gcc[i] != gccs[i] for i in range(n)]),
            #, vv != v
            )
            )

        print "num_solutions:", num_solutions
        
