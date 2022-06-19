"""
0/1 Knapsack in cpmpy.

From http://rosettacode.org/wiki/Knapsack_problem/0-1
'''
A tourist wants to make a good trip at the weekend with his friends.
They will go to the mountains to see the wonders of nature, so he 
needs to pack well for the trip. He has a good knapsack for carrying 
things, but knows that he can carry a maximum of only 4kg in it and 
it will have to last the whole day. He creates a list of what he
wants to bring for the trip but the total weight of all items is too 
much. He then decides to add columns to his initial list detailing 
their weights and a numerical value representing how important the item is for the trip.

Here is the list:
Table of potential knapsack items item 	weight (dag) 	value
map 	9 	150
compass 	13 	35
water 	153 	200
sandwich 	50 	160
glucose 	15 	60
tin 	68 	45
banana 	27 	60
apple 	39 	40
cheese 	23 	30
beer 	52 	10
suntan cream 	11 	70
camera 	32 	30
T-shirt 	24 	15
trousers 	48 	10
umbrella 	73 	40
waterproof trousers 	42 	70
waterproof overclothes 	43 	75
note-case 	22 	80
sunglasses 	7 	20
towel 	18 	12
socks 	4 	50
book 	30 	10
knapsack 	<=400 dag 	 ?

The tourist can choose to take any combination of items from the
list, but only one of each item is available. He may not cut or
diminish the items, so he can only take whole units of any item.

Which items does the tourist carry in his knapsack so that their
total weight does not exceed 400 dag [4 kg], and their total value 
is maximised?
'''

These are the items to pick:
  Item                    Weight Value
* map                          9 150
* compass                     13  35
* water                      153 200
* sandwich                    50 160
* glucose                     15  60
* banana                      27  60
* suntancream                 11  70
* waterproof trousers         42  70
* waterproof overclothes      43  75
* note-case                   22  80
* sunglasses                   7  20
* socks                        4  50

Total weight: 396
Total value: 1030


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def knapsack_rosetta_code_01(items,max_total_weight):

  n = len(items)
  weights = [items[i][1] for i in range(n)]
  values = [items[i][2] for i in range(n)]  

  # Variables
  x = boolvar(shape=n,name="x")
  total_weight = intvar(0,sum(weights),name="total_weights")
  total_value = intvar(0,sum(values),name="total_values")  

  # Constraints
  model = Model([total_weight == (x*weights).sum(),
                 total_value == (x*values).sum(),
                 total_weight <= max_total_weight,
                 ])

  model.maximize(total_value)

  def print_sol(): 
    print("x:",x.value())
    print("We pick these items:")
    print(f"  {'Item':22s} {'Weight':7s} {'Value':6s}")
    [print(f"* {items[i][0]:22s} {items[i][1]:6} {items[i][2]:6}") for i in range(n) if x[i].value()]
    print()
    print("Total_weight:",total_weight.value())
    print("Total_value:",total_value.value())
    print()
  

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)


#          Item          Weight   Value
items = [["map",           9,      150],
         ["compass",       13,      35],
         ["water",         153,    200],
         ["sandwich",      50,     160],
         ["glucose",       15,      60],
         ["tin",           68,      45],
         ["banana",        27,      60],
         ["apple",         39,      40],
         ["cheese",        23,      30],
         ["beer",          52,      10],
         ["suntancream",   11,      70],
         ["camera",        32,      30],
         ["T-shirt",       24,      15],
         ["trousers",      48,      10],
         ["umbrella",      73,      40],
         ["waterproof trousers",     42,      70],
         ["waterproof overclothes",  43,      75],
         ["note-case",     22,      80],
         ["sunglasses",     7,      20],
         ["towel",         18,      12],
         ["socks",         4,       50],
         ["book",          30,      10]]

max_total_weight = 400
knapsack_rosetta_code_01(items,max_total_weight)
