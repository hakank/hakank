# 
# Shopping with delivery cost in Z3
#
# (This is a port of my MiniZinc model http://hakank.org/minizinc/shopping_with_delivery_costs.mzn)
#
# From https://groups.google.com/forum/embed/?place=forum/minizinc&showsearch=true&showpopout=true&showtabs=false&parenturl=http#3A#2F#2Fwww.minizinc.org#2Fforum.html#!topic/minizinc/pJaCxrVVzq8
# """
# Gabriel Fazzio de Paula
#
# Hello everyone!
#
# I'd like to start by saying I'm a complete noob in Minizinc. I've been battling with a problem 
# for a while now and a awesome reddit user mentioned Minizinc, and he/she said that it's the 
# perfect tool for the job and offered to help me, however, I had to give it a try by myself 
# first. Unfortunately, I was not able to solve it and too much time has past and I'm afraid 
# I'll never get that help again, so I'm here asking for help!
#
# The problems is: imagine I want to buy different kinds of parts from a set of different stores, 
# and each store could or couldn't have each specific part, and I want to minimize the cost of 
# buying all parts. The tricky part is that for each store I'd need to pay the shipping cost and 
# sometimes buying a part at the lower price, means buying it at a store that the shipping would 
# make it more expensive than buying it from a store that is more expensive but have other less 
# expensive parts. Here's some example data that might help
#
# XXXXXX	STORE_A	STORE_B	STORE_C			
# PART_A	N/A	2.25	2.9		STORE_A_DELIVERY_COST	12.56
# PART_B	N/A	3	N/A		STORE_B_DELIVERY_COST	15.2
# PART_C	2	15	7		STORE_C_DELIVERY_COST	33.5
#
# I'd like to find the minimum price to buy all parts including the delivery cost.
#
# What I've attempted to do (and probably is completely incorrect) is to make a decision variable 
# that would be either 1 or 0 for each Store,Part pair, and I'd minimize it for each Store, Part 
# pair, but I couldn't make it work.
#
# Thank you!
# """

# Solution:
# x: [1, 1, 0]
# total_costs: 35.01
# part_cost: 7.25
# delivery_cost: 27.76
# 
# I.e.
# - buy product 0 and 1 from store 1
# - buy product 2 from store 0
# for a total cost of 35.01
# 
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from z3 import *


def shopping_with_delivery_costs(product_stores, store_delivery_costs):

  s = Optimize() 
  # s = SimpleSolver()
  # s = Solver()

  num_parts = len(product_stores)
  num_stores = len(product_stores[0])

  # for the element constraints in product_stores we use a Function:
  #    product_stores_a(i,j) = product_stores[i][j]
  product_stores_a = Function("product_stores_a", IntSort(), IntSort(), RealSort())
  for p in range(num_parts):
    for st in range(num_stores):
      s.add(product_stores_a(p,st) == product_stores[p][st])

  #
  # decision variables
  #
  
  # which store to buy product p?
  x = [Int(f"x[{p}]") for p in range(num_parts)]
  for p in range(num_parts):
    s.add(x[p] >= 0, x[p] < num_stores)

  part_cost, delivery_cost, total_costs = Reals("part_cost delivery_cost total_cost")
  s.add(total_costs == part_cost + delivery_cost)

  # a selected part must be in an available store
  for p in range(num_parts):
    s.add(product_stores_a(p,x[p]) > 0)


  # total cost for the parts
  s.add(part_cost == Sum([product_stores_a(p,x[p]) for p in range(num_parts)]))


  # the delivery cost per store (per order)
  s.add(delivery_cost == Sum([
                    # is there something bought from shop st?
                    If(Sum([If(x[p] == st,1,0) for p in range(num_parts)]) > 0, store_delivery_costs[st], 0)
                    for st in range(num_stores)]))

  s.minimize(total_costs)
  
  num_solutions = 0
  if s.check() == sat:
    num_solutions += 1
    mod = s.model()
    print("x:", [mod[x[p]] for p in range(num_parts)])
    print("total_costs:", mod[total_costs].as_decimal(6))
    print("part_cost:", mod[part_cost].as_decimal(6))
    print("delivery_cost:", mod[delivery_cost].as_decimal(6))
    print()

  print("num_solutions:", num_solutions)

# product_stores[part,store]: price 0.0 means that p is _not_ available at store s
product_stores = [[0.0,  2.25, 2.9],
                  [0.0,  3.00, 0.0],
                  [2.0, 15.00, 7.0]]

# delivery cost per store (per order)
store_delivery_costs = [12.56, 15.2, 33.5]


shopping_with_delivery_costs(product_stores, store_delivery_costs)
