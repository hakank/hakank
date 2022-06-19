"""
Knapsack (bounded) in cpmpy.

From 
http://rosettacode.org/wiki/Knapsack_problem/Bounded
'''
A tourist wants to make a good trip at the weekend with his 
friends. They will go to the mountains to see the wonders of 
nature. So he needs some items during the trip. Food, clothing, 
etc. He has a good knapsack for carrying the things, but he knows 
that he can carry only 4 kg weight in his knapsack, because they 
will make the trip from morning to evening. He creates a list of 
what he wants to bring for the trip, but the total weight of all 
items is too much. He adds a value to each item. The value represents 
how important the thing for the tourist. The list contains which 
items are the wanted things for the trip, what is the weight and 
value of an item, and how many units does he have from each items.

This is the list:
Table of potential knapsack items item 	weight (dag) (each) 	value (each) 	piece(s)
map 	9 	150 	1
compass 	13 	35 	1
water 	153 	200 	2
sandwich 	50 	60 	2
glucose 	15 	60 	2
tin 	68 	45 	3
banana 	27 	60 	3
apple 	39 	40 	3
cheese 	23 	30 	1
beer 	52 	10 	3
suntan cream 	11 	70 	1
camera 	32 	30 	1
T-shirt 	24 	15 	2
trousers 	48 	10 	2
umbrella 	73 	40 	1
waterproof trousers 	42 	70 	1
waterproof overclothes 	43 	75 	1
note-case 	22 	80 	1
sunglasses 	7 	20 	1
towel 	18 	12 	2
socks 	4 	50 	1
book 	30 	10 	2
knapsack 	<=400 dag 	 ? 	 ? 


The tourist can choose to take any combination of items from the 
list, and some number of each item is available (see the column 
'Piece(s)' of the list!). He may not cut the items, so he can only 
take whole units of any item.

Which items does the tourist carry in his knapsack so that their 
total weight does not exceed 4 kg, and their total value is maximised?
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def knapsack_rosetta_code_bounded(items,max_total_weight):

  n = len(items)
  weights = [items[i][1] for i in range(n)]
  values  = [items[i][2] for i in range(n)]
  pieces  = [items[i][3] for i in range(n)]    

  # Variables
  x = intvar(0,max(pieces),shape=n,name="x")
  total_weight = intvar(0,sum(weights),name="total_weights")
  total_value  = intvar(0,sum(values),name="total_values")  

  # Constraints
  model = Model([total_weight == (x*weights).sum(),
                 total_value == (x*values).sum(),
                 [x[i] <= pieces[i] for i in range(n)],
                 total_weight <= max_total_weight,
                 ])

  model.maximize(total_value)

  def print_sol():
    print("x:",x.value())
    print("We pick these items:")
    print(f"  {'Item':26s} {'Weight':7s} {'Value':6s}")
    [print(f"* {x[i].value()} of {items[i][0]:22s} {items[i][1]:5} {items[i][2]:6}") for i in range(n) if x[i].value()]
    print()
    print("Total_weight:",total_weight.value())
    print("Total_value:",total_value.value())
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

#         Item          Weight   Value  Pieces
items = [["map",            9,     150,    1],
         ["compass",       13,      35,    1],
         ["water",        153,     200,    2],
         ["sandwich",      50,      60,    2],
         ["glucose",       15,      60,    2],
         ["tin",           68,      45,    3],
         ["banana",        27,      60,    3],
         ["apple",         39,      40,    3],
         ["cheese",        23,      30,    1],
         ["beer",          52,      10,    3],
         ["suntancream",   11,      70,    1],
         ["camera",        32,      30,    1],
         ["T-shirt",       24,      15,    2],
         ["trousers",      48,      10,    2],
         ["umbrella",      73,      40,    1],
         ["waterproof trousers",    42,      70,    1],
         ["waterproof overclothes", 43,      75,    1],
         ["note-case",     22,      80,    1],
         ["sunglasses",     7,      20,    1],
         ["towel",         18,      12,    2],
         ["socks",          4,      50,    1],
         ["book",          30,      10,    2]]

max_total_weight = 400
knapsack_rosetta_code_bounded(items,max_total_weight)
