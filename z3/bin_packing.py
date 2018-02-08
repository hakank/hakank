#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Cbin_packing.html
# """
# Given several items of the collection ITEMS (each of them having a specific 
# weight), and different bins of a fixed capacity, assign each item to a bin 
# so that the total weight of the items in each bin does not exceed CAPACITY.
# 
# Example
#   <(5,<bin-3 weight-4, bin-1 weight-3,bin-3 weight-1>)>
# 
#  The bin_packing constraint holds since the sum of the height of items 
# that are assigned to bins 1 and 3 is respectively equal to 3 and 5. 
# The previous quantities are both less than or equal to the maximum 
# CAPACITY 5. Figure 4.35.1 shows the solution associated with the example.
# 
# Remark
# 
# Note the difference with the classical bin-packing problem [MT90] where 
# one wants to find solutions that minimise the number of bins. In our 
# case each item may be assigned only to specific bins (i.e., the different 
# values of the bin variable) and the goal is to find a feasible solution. 
# This constraint can be seen as a special case of the cumulative 
# constraint [AB93], where all task durations are equal to 1.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


#
# bin_packing
#
# Note: capacity (and bins) might be IntVar but weights must be an int vector
#
def bin_packing(sol,capacity, bins, weights):
  n = len(bins)
  for b in range(n):
    sol.add(Sum([ weights[j]*If(bins[j] == b,1,0) for j in range(n)] ) <= capacity)


#
# print the packing
#
def print_packing(weights,bins):
    n = len(weights)
    for i in range(n):
      total = 0
      packed = []
      for j in range(n):
        if bins[j] == i:
          total += weights[j]
          packed.append(j)
      if total > 0:
        print "bin %2i"%i , " items:",  packed, " total weight:", total
    print

#
# minimize the capacity (if capacity1 is not set)
#
def test_bin_packing_min_capacity(weights,capacity1=None):

  sol = Solver()

  n = len(weights)

  for i in range(n):
    assert weights[i] <= capacity, "All weights must be <= capacity: "

  # variables
  bins = makeIntVector(sol,"bins",n,0,n-1)

  capacity = Int("capacity")
  if capacity1 != None:
    sol.add(capacity == capacity1)
  else:
    sol.add(capacity >= max(weights))

  # constraints
  bin_packing(sol, capacity, bins, weights)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print "weights:", weights, "capacity  :", mod.eval(capacity)
    print "bins :", [mod.eval(bins[i]) for i in range(n)]
    bbins = [mod.eval(bins[i]) for i in range(n)]
    print_packing(weights,bbins)
    
    if capacity1 == None:
      getLessSolution(sol,mod,capacity)
    else:
      getDifferentSolution(sol,mod,bins)

  print "num_solutions:", num_solutions  

#
# Minimize the number of bins
# Here both weights and capacity must be ints
#
def test_bin_packing_min_bins(weights,capacity):

  sol = Solver()
  # sol = Optimize()

  n = len(weights)
  # for i in range(n):
  #  assert weights[i] <= capacity, "All weights must be <= capacity: "



  # variables
  bins = makeIntVector(sol,"bins",n,0,n-1)
  weight_per_bin = makeIntVector(sol,"weight_per_bin", n, 0, capacity)

  max_bin_used = makeIntVar(sol,"max_bin_used", 0,n-1)

  # constraints
  
  # find the max number of bin used
  maximum(sol,max_bin_used,bins)

  # bin_packing(sol, capacity, bins, weights)
  for b in range(n):
    # this includes the bin_packing constraint
    sol.add(weight_per_bin[b] == Sum([ weights[j]*If(bins[j] == b,1,0) for j in range(n)] ))
    sol.add(weight_per_bin[b] <= capacity)

  decreasing(sol,weight_per_bin)

  # sol.minimize(max_bin_used)
  num_solutions = 0
  while sol.check() == sat:
  # if sol.check() == sat:      
    num_solutions += 1
    mod = sol.model()
    print "weights:", weights
    print "capacity  :", capacity
    print "max_bin_used:", mod.eval(max_bin_used)
    print "weight_per_bin:", [mod.eval(weight_per_bin[i]) for i in range(n)]
    bbins = [mod.eval(bins[i]) for i in range(n)]
    print "bins :", bbins
    print_packing(weights,bbins)
    print
    getLessSolution(sol,mod,max_bin_used)

  print "num_solutions:", num_solutions  


# n = 3
# weights = [4,3,1]
# capacity = 4

# simple (and probably unrealistic) packing
# capacity = 20
weights = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
capacity = 30


# The problem below is from
# http://www.dcs.gla.ac.uk/~pat/cpM/slides/binPacking.ppt
# 1D Bin Packing (or "CP? Who cares?"), page 3
# and also from
# http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/binPack
#
# num_stuff = 10;
weights = [42,63,67,57,93,90,38,36,45,42]
capacity = 150;
# capacity = 100;


# This problem instance is via
# http://yetanothermathprogrammingconsultant.blogspot.com/2011/08/bin-packing.html
# and is "critical" to the result in
# "Bin Packing, What is It?"
# http://www.developerfusion.com/article/5540/bin-packing/
# where the minimum bins are 19,
# whereas Erwin K's code have 17 as the optimal result.
# 
# LazyFD solves this in 1:44 minutes (i.e. with result 17) 
# when num_bins is hardcoded to 19 (as in Erwin's model)
# 
weights = [
 26,   57, 18,   8,  45,
 16,   22, 29,   5,  11,
  8,   27, 54,  13,  17,
 21,   63, 14,  16,  45,
  6,   32, 57,  24,  18,
 27,   54, 35,  12,  43,
 36,   72, 14,  28,   3,
 11,   46, 27,  42,  59,
 26,   41, 15,  41,  68]
capacity = 80

# same source of data, but now there are 22 things
# num_stuff = 22; % number of things
# weights = [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,85,33]
# capacity = 250

# still more stuff
# weights = [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,41,49,55,98,80,32,25,38,82,30] # 35,39,57,84,62,50,55,27,30,36,20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,3]
# capacity = 290

# From 
# Graham Kendall: Bin Packing made Easier 
# http://research-reflections.blogspot.com/2009/07/bin-packing-made-easier.html
weights = [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
           252,252,127,85,12,10,9,252,127,106,84,12,10,252,127,106,46,12,10]
capacity = 524


# # Variant: remove 46 from the problem above
# weights = [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
#            252,252,127,85,12,10,9,252,127,106,84,12,10,252,
#            127,106,12,10]
# capacity = 524



# test_bin_packing(weights)
# test_bin_packing(weights,capacity)
test_bin_packing_min_bins(weights,capacity)
