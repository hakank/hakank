#!/usr/bin/python -u
# -*- coding: latin-1 -*-
#
# Utilities and (decompositions of) global constraints in Z3.
#
# Here I have collected some useful (or perhaps not that useful) methods for z3py.
# These where added mostly for simplifying the porting of my "traditional"
# constraint programming models.
#
#
#####################################################################
# Convenience wrappers for creating variables and their domains etc #
#####################################################################
#
# - makeIntVar(sol,name,min_val, max_val)
# - makeIntVarVals(sol,name,vals)
# - makeIntVars(sol,name,size, min_val, max_val)
# - makeIntVector(sol,name,min_val, max_val)
# - makeIntVectorMatrix(sol,name,rows,cols,min_value,max_value)
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
# - copyArray(sol,a1,name, min_val, max_val)
# - copyArrayMatrix(sol,a1,name, rows, cols, min_val, max_val)
#

#
############################################# 
# Global constraints (decompositions) in Z3 #
#############################################
#
# - all_different(sol,x)
# - all_different_except_0(sol,x)
# - element(sol,ix,x,v,n)
# - element_matrix(sol,ix,jx,x,v,rows,cols)
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
# - v == maximum2(sol,x)
# - minimum(sol,min,x)
# - v == minimum2(sol,x)
# - abs(x)
# - toNum
# - subset_sum(sol, values, total)
# - allowed_assignments(sol,t,allowed)  aka table, table_in
# - member_of(sol, e, v)
# - no_overlap(sol, s1, d1, s2, d2)
# - sliding_sum(sol, low, up, seq, x)
# - bin_packing(sol,capacity, bins, weights)
# - cumulative(sol, s, d, r, b,times_min,times_max1)
# - global_contiguity(sol, x,start,end)
# - regular(sol, x, Q, S, d, q0, F, x_len)
# - all_different_modulo(sol, x, m)
# - among(sol,m,x,v)
# - nvalue(sol, m, x, min_val,max_val)
# - clique(sol, g, clique, card)
# - all_min_dist(sol,min_dist, x, n)
# - all_different_cst(sol, xs, cst)
# - all_different_on_intersection(sol, x, y)
# - all_different_pairs(sol, a, s)
# - increasing_pairs(sol,a, s)
# - decreasing_pairs(sol,a, s)
# - pairs(sol, a, s)
# - all_differ_from_at_least_k_pos(sol, k, x)
# - all_differ_from_exact_k_pos(sol, k, vectors)
# - all_differ_from_at_most_k_pos(sol, k, x)
# - all_equal(sol,x)
# - arith(sol, x, relop, val)
# - arith_relop(sol, a, t, b)
#
# 
# TODO
# lex_(le|lt|ge|gt)(sol,x,y)  : array x is lexicographic (equal or) less/greater than array y
# diffn?
# subcircuit???
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function

from z3 import *
import uuid
import time

def getNewId():
  return uuid.uuid4().int

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

def makeIntVectorMatrix(sol,name,rows,cols,min_value,max_value):
  x = {}
  for i in range(rows):
    for j in range(cols):
      x[(i,j)] = makeIntVar(sol,name + "%i_%i"%(i,j),min_value,max_value)
  return x

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

# creates an Array with a domain
def makeRealArray(sol,name, size, min_val, max_val):
    a = Array(name,RealSort(),RealSort())
    [sol.add(a[i] >= min_val, a[i] <= max_val) for i in range(size)]
    return a


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
            print(mod.eval(x[(i,j)]), end=' ')
        print()
    print()

#
# Copy the (integer) array into an Array()
#
def copyArray(sol,a1,name, min_val, max_val):
  n = len(a1)
  a = makeIntArray(sol,name,n,min_val,max_val)
  for i in range(n):
    sol.add(a[i] == a1[i])
  return a

#
# Copy the (integer) array into an Array()
#
def copyRealArray(sol,a1,name, min_val, max_val):
  n = len(a1)
  a = makeRealArray(sol,name,n,min_val,max_val)
  for i in range(n):
    sol.add(a[i] == a1[i])
  return a


#
# Copy the (integer) matrix into an Array()
#
def copyArrayMatrix(sol,a1,name, rows, cols, min_val, max_val):
  a = makeIntArray(sol,name,rows*cols,min_val,max_val)
  for i in range(rows):
    for j in range(cols):        
      sol.add(a[i*cols+j] == a1[i][j])
  return a


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

#
# element(sol,ix,x,v,n)
#  v = x[ix]
# n = length of x
#
# Experimental!
#
def element(sol,ix,x,v,n):
  for i in range(n):
    sol.add(Implies(i==ix, v == x[i]))


#
# element_matrix(sol,ix,jx,x,v,rows,cols)
#   v = x[(ix,jx)]
# where x is an matrix of rows x cols
#
# Experimental!
#
def element_matrix(sol,ix,jx,x,v,rows,cols):
  for i in range(rows):
    for j in range(cols):
      sol.add(Implies(And(i == ix, j == jx), v == x[(i,j)]))



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

# v is the maximum value of x
def maximum(sol, v, x):
  sol.add(Or([v == x[i] for i in range(len(x))])) # max is an element in x)
  for i in range(len(x)):
    sol.add(v >= x[i]) # and it's the greatest

# v == maximum2(sol,x): v is the maximum value of x
def maximum2(sol, x):
  v = Int("v_%i"% uuid.uuid4().int)
  sol.add(Or([v == x[i] for i in range(len(x))])) # v is an element in x)
  for i in range(len(x)):
    sol.add(v >= x[i]) # and it's the greatest
  return v


# min is the minimum value of x
def minimum(sol, v, x):
  sol.add(Or([v == x[i] for i in range(len(x))])) # v is an element in x)
  for i in range(len(x)):
    sol.add(v <= x[i]) # and it's the smallest

# v == minimum2(sol,x): v is the minimum value of x
def minimum2(sol, x):
  v = Int("v_%i"% uuid.uuid4().int)
  sol.add(Or([v == x[i] for i in range(len(x))])) # min is an element in x)
  for i in range(len(x)):
    sol.add(v <= x[i]) # and it's the smallest
  return v


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

# bin_packing
#
# Note: capacity (and bins) might be IntVar but weights must be an int vector
#
def bin_packing(sol,capacity, bins, weights):
  n = len(bins)
  for b in range(n):
    sol.add(Sum([ weights[j]*If(bins[j] == b,1,0) for j in range(n)] ) <= capacity)

#
# Decompositon of cumulative.
#
# Inspired by the MiniZinc implementation:
# http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=g12:zinc:lib:minizinc:std:cumulative.mzn&s[]=cumulative
# The MiniZinc decomposition is discussed in the paper:
# A. Schutt, T. Feydy, P.J. Stuckey, and M. G. Wallace.
# 'Why cumulative decomposition is not as bad as it sounds.'
# Download:
# http://www.cs.mu.oz.au/%7Epjs/rcpsp/papers/cp09-cu.pdf
# http://www.cs.mu.oz.au/%7Epjs/rcpsp/cumu_lazyfd.pdf
#
#
# Parameters:
#
# s: start_times    assumption: array of IntVar
# d: durations      assumption: array of int
# r: resources      assumption: array of int
# b: resource limit assumption: IntVar or int
#
# Note: since I don't know how to extract the bounds of the
#       domains, both times_min and times_max1 are required
#       which is the lower/upper limits of s (the start_times).
#       Which makes it slower...
#
def cumulative(sol, s, d, r, b,times_min,times_max1):

  tasks = [i for i in range(len(s)) if r[i] > 0 and d[i] > 0]
  
  # how do I get the upper/lower value of a decision variable?
  # times_min = min([s[i].Min() for i in tasks])
  # times_max = max([s[i].Max() + max(d) for i in tasks])
  times_max = times_max1 + max(d)
  for t in range(times_min, times_max + 1):
    for i in tasks:
      sol.add(Sum([(If(s[i] <= t,1,0) * If(t < s[i] + d[i],1,0))*r[i] for i in tasks])  <= b)

  # Somewhat experimental:
  # This constraint is needed to contrain the upper limit of b.
  if not isinstance(b, int):
    sol.add(b <= sum(r))


#
# Global_contiguity:
# Enforce that all 1s must be in a contiguous group.
# Assumption: There must be at least one 1.
#
def global_contiguity(sol, x,start,end):
  n = len(x)
  sol.add(start<=end)
  for i in range(n):
    sol.add(And(i >= start, i <= end) == x[i] == 1)


#
# Global constraint regular
#
# This is a translation of MiniZinc's regular constraint (defined in
# lib/zinc/globals.mzn), via the Comet code refered above.
# All comments are from the MiniZinc code.
# '''
# The sequence of values in array 'x' (which must all be in the range 1..S)
# is accepted by the DFA of 'Q' states with input 1..S and transition
# function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
# (which must be in 1..Q) and accepting states 'F' (which all must be in
# 1..Q).  We reserve state 0 to be an always failing state.
# '''
#
# x : IntVar array
# Q : number of states
# S : input_max
# d : transition matrix
# q0: initial state
# F : accepting states
# x_len: length of x [when using Array we cannot extract the length]
#
def regular(sol, x, Q, S, d, q0, F, x_len):

  assert Q > 0, 'regular: "Q" must be greater than zero'
  assert S > 0, 'regular: "S" must be greater than zero'

  # d2 is the same as d, except we add one extra transition for
  # each possible input;  each extra transition is from state zero
  # to state zero.  This allows us to continue even if we hit a
  # non-accepted input.

  # Comet: int d2[0..Q, 1..S]
  d2 = []
  for i in range(Q + 1):
    row = []
    for j in range(S):
      if i == 0:
        row.append(0)
      else:
        row.append(d[i - 1][j])
    d2.append(row)

  d2_flatten = [d2[i][j] for i in range(Q + 1) for j in range(S)]
  d2_flatten_a = makeIntArray(sol,"d2_flatten_a_%i"%uuid.uuid4().int,len(d2_flatten),min(d2_flatten),max(d2_flatten))
  for i in range(len(d2_flatten)):
     sol.add(d2_flatten[i] == d2_flatten_a[i])

  # If x has index set m..n, then a[m-1] holds the initial state
  # (q0), and a[i+1] holds the state we're in after processing
  # x[i].  If a[n] is in F, then we succeed (ie. accept the
  # string).
  x_range = list(range(0, x_len))
  m = 0
  # n = len(x)
  n = x_len

  a = [makeIntVar(sol,'a[%i]_%i' % (i,uuid.uuid4().int), 0, Q + 1) for i in range(m, n + 1)]

  # Check that the final state is in F
  member_of(sol,a[-1],F)
  
  # First state is q0
  sol.add(a[m] == q0)
  for i in x_range:
    sol.add(x[i] >= 1)
    sol.add(x[i] <= S)

    # Determine a[i+1]: a[i+1] == d2[a[i], x[i]]
    sol.add(a[i + 1] == d2_flatten_a[(a[i] * S) + (x[i] - 1)])

#
# all_different_modulo(sol, x, m)
#
# Ensure that all elements in x (modulo m) are distinct
# 
def all_different_modulo(sol, x, m):
  n = len(x)
  mods = makeIntVector(sol,"mods",n, 0,m-1)
  for i in range(n):
     sol.add(mods[i] == x[i] % m)
  sol.add(Distinct(mods))


# among(sol,m,x,v)
#
# Requires exactly m variables in x to take one of the values in v.
# 
def among(sol,m,x,v):
  sol.add(m == Sum([If(x[i] == j,1,0) for i in range(len(x)) for j in v]))


# nvalue(sol, m, x, min_val,max_val)
# 
# Requires that there is exactly m distinct values in x
# (min_val and max_val are the minimum and maximum value
# in x, respectively)
#
def nvalue(sol, m, x, min_val,max_val):
  n = len(x)
  sol.add(m == Sum([ If(Sum([ If(x[j] == i,1,0) for j in range(n)]) > 0,1,0) for i in range(min_val, max_val+1)]))



#
# clique(sol, g, clique, card)
#
# Ensure that the boolean array "clique" (of Integer Array type) 
# represents a clique in the graph g with the cardinality card.
# 
# Note: This is kind of backward, but it is the whole thing:
# If there is a connection between nodes I and J (I \= J) then
# there should be a node from I to J in G. If it's not then
# both c1 and c2 is not in the clique.
#
def clique(sol, g, clique, card):
  n = len(g)
  sol.add(card == Sum([clique[i] for i in range(n)]))
  for (c1,i) in zip(clique, range(n)):
    for (c2,j) in zip(clique, range(n)):
      sol.add(Implies(And(i != j, g[i][j] == 0), Or(c1 == 0, c2 == 0)))


#
# all_min_dist(sol,min_dist, x, n)
#
# Ensures that the differences of all pairs (i !=j) are
# >= min_dist.
#
def all_min_dist(sol,min_dist, x, n):
  for i in range(n):
    for j in range(i):
      sol.add(Abs(x[i]-x[j]) >= min_dist)

#
# Ensure that all elements in xs + cst are distinct
#
def all_different_cst(sol, xs, cst):
    sol.add(Distinct([(x + c) for (x,c) in zip(xs,cst)]))

#
# Ensure that the values that are common in x and y are distinct (in each array)
#
def all_different_on_intersection(sol, x, y):
    _count_a_in_b(sol,x,y)
    _count_a_in_b(sol,y,x)

# helper for all_different_on_intersection
def _count_a_in_b(sol,ass,bss):
    for a in ass:
        sol.add(Sum([If(a == b,1,0) for b in bss]) <= 1)


# all pairs must be different
def all_different_pairs(sol, a, s):
    sol.add(Distinct([p for p in pairs(sol,a,s)]))

# the pairs are in increasing order
def increasing_pairs(sol,a, s):
    increasing(sol,pairs(sol,a,s)) 
  
# the pairs are in decreasing order
def decreasing_pairs(sol,a, s):
    decreasing(sol,pairs(sol,a,s)) 

# return the pairs of a in the "integer representation": a[k,0]*(n-1) + a[k,1]
# s is the size of max value of n
def pairs(sol, a, s):
    n = len(a)//2
    return [ a[(k,0)]*(s-1) + a[(k,1)] for k in range(n)]


#
# all_differ_from_at_least_k_pos(sol, k, x)
#
# Ensure that all pairs of vectors has >= k different values
#
def all_differ_from_at_least_k_pos(sol, k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    for i in range(n):
        for j in range(i+1,n):
            sol.add(Sum([If(vectors[i][kk] != vectors[j][kk],1,0) for kk in range(m)]) >= k)

#
# all_differ_from_exact_k_pos(sol, k, vectors)
#
# Ensure that all pairs of vectors has exactly k different values
#
def all_differ_from_exact_k_pos(sol, k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    for i in range(n):
        for j in range(i+1,n):
            sol.add(Sum([If(vectors[i][kk] != vectors[j][kk],1,0) for kk in range(m)]) == k)

#
# all_differ_from_at_most_k_pos(sol, k, x)
#
# Ensure that all pairs of vectors has <= k different values
#
def all_differ_from_at_most_k_pos(sol, k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    for i in range(n):
        for j in range(i+1,n):
            sol.add(Sum([If(vectors[i][kk] != vectors[j][kk],1,0) for kk in range(m)]) <= k)

#
# all values in x must be equal
#
def all_equal(sol,x):
    sol.add(And([x[i] == x[i-1] for i in range(len(x))]))


#
# Ensure that all elements in x are <relop> then val.
#
def arith(sol, x, relop, val):
    for i in range(len(x)):
        arith_relop(sol,x[i],relop, val)


#
# This is (arguably) a hack.
# Represents each function as an integer 0..5.
# 
def arith_relop(sol, a, t, b):
    sol.add(Implies(t == 0,a  < b))
    sol.add(Implies(t == 1,a <= b))
    sol.add(Implies(t == 2,a == b))
    sol.add(Implies(t == 3,a >= b))
    sol.add(Implies(t == 4,a  > b))
    sol.add(Implies(t == 5,a  != b))



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
    print(sol.check())
    while sol.check() == sat:
        num_solutions = num_solutions + 1
        mod = sol.model()
        ss = [mod.eval(x[i]) for i in range(n)]
        cc = mod.eval(c)
        # vv = m.eval(v)
        gccs = ([mod.eval(gcc[i]) for i in range(n)])
        # print(ss, " #0s: ", mod.eval(cc), " v:", m.eval(v))
        print(ss, " #0s: ", mod.eval(cc), " gcc:", gccs)
        sol.add(
            Or(
            Or([x[i] != ss[i] for i in range(n)]),
            cc != c 
            , Or([gcc[i] != gccs[i] for i in range(n)]),
            #, vv != v
            )
            )

        print("num_solutions:", num_solutions)
        


#
# diffn ported from MiniZinc's fzn_diffn:
# 
# predicate fzn_diffn(array[int] of var int: x,
#                 array[int] of var int: y,
#                 array[int] of var int: dx,
#                 array[int] of var int: dy) =
#     forall(i,j in index_set(x) where i < j)(
#         x[i] + dx[i] <= x[j] \/ y[i] + dy[i] <= y[j] \/
#         x[j] + dx[j] <= x[i] \/ y[j] + dy[j] <= y[i]
#     );
#
def diffn(sol,x,y,dx,dy):
    n = len(x)
    for i in range(n):
        for j in range(i+1,n):
            sol.add(
                Or([x[i] + dx[i] <= x[j],
                    y[i] + dy[i] <= y[j],
                    x[j] + dx[j] <= x[i],
                    y[j] + dy[j] <= y[i]]
                   )
                )
