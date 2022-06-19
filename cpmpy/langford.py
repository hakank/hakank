"""
Langford's number problem in cpmpy.

Langford's number problem (CSP lib problem 24)
http://www.csplib.org/prob/prob024/
'''
Arrange 2 sets of positive integers 1..k to a sequence,
such that, following the first occurence of an integer i,
each subsequent occurrence of i, appears i+1 indices later
than the last.
For example, for k=4, a solution would be 41312432
'''

* John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html

* https://en.wikipedia.org/wiki/Langford_pairing

* http://dialectrix.com/langford.html

* Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


For a solution to be possible this must hold:
    k % 4 == 0 or k % 4 == 3

Here's a solution of k = 159, solved in 4.1s
'''
solution: [ 43  30  59  80  93 105 153  91  12 133  55  41 116  67  16 108 144 141
   3  19 132  12   3  33 117   8 122  75  50 114  87  16  30  92   8 148
  53  74  86  19  89  71  11 157  43 102  99 131 155  85 127  44 119  41
  11 137 101  33  88 149   7  77  59  81  36   6  55 112   7 142   4 103
   6  46  10   4 138  98 111  50  60  67 113 124  80  10   5 106  62  48
  53 128   5 125 123  37  44 109  93  91  90  36 120  75  95  25 147 143
 121 140 135 105  74  71  66  49 159 146  87 150  46  32  51 129 108  86
  92 134 151 116  89  25  84  37 110  85 154 130  48  77  58  60 117 133
 114  81  99  88 102 122 152  62 156 132  32 158 136 139 101 141 153 144
  14  79 107  49 115 145  26  18  69 118 119  73  51 103  98  14 127 131
 112  66  65 104 148  83  34  96  18 126 111  90  35 137 106  26 113  94
  76  58  95 157  70  82 155  61  42 109 124 149  24  21 142  97 100 138
  27  84 123 125 128  34  40 120  31  45  39  29  35  72 121  63  68  21
  54  24  28  47  78  56  69  13  64  79  27 110 135  73  65  42 140 143
  57 129 147  13  31  29  20  52  22  38 134  40 146  28  39  61 130  83
 150  45 107  70  23  76 159   9  15  20 151  17 115  22  96  47  82   9
 104  54 118 154  94 136  15  63  56 139  23  17  38  68  72 152   1   2
   1  64   2 156  57  97  52 145 158 100 126  78]
True
ExitStatus.FEASIBLE (2.8419967500000003 seconds)
Nr solutions: 1
Num conflicts: 1149
NumBranches: 192184
WallTime: 2.8419967500000003
python3 langford.py 159 1  5,06s user 1,37s system 155% cpu 4,124 total
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def langford(k=8, num_sols=0):

  model = Model()

  #
  # data
  #
  print("k:", k)
  
  if not (k % 4 == 0 or k % 4 == 3):
    print("There is no solution for K unless K mod 4 == 0 or K mod 4 == 3")
    return
  p = list(range(2 * k))

  # variables
  position = intvar(0,2*k-1,shape=2*k,name="position")
  solution = intvar(1,k,shape=2*k,name="solution")

  # constraints
  model += [AllDifferent(position)]

  for i in range(1, k + 1):
    model += [position[i+k-1] == position[i-1]+i+1]
    model += [i == solution[position[i-1]]]
    model += [i == solution[position[k+i-1] ]]        

  # symmetry breaking
  model += [solution[0] < solution[2 * k - 1]]

  def print_sol():
      print("position:", position.value())
      print("solution:", solution.value())
      print()

  num_solutions = model.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

def benchmark():
  """
  Benchmark langford for k = 1..200
  """
  for k in range(1,201):
    main(k, 1)
    print()


k = 8
num_sols = 0
if __name__ == "__main__":
  if len(sys.argv) > 1:
    k = int(sys.argv[1])
  if len(sys.argv) > 2:
    num_sols = int(sys.argv[2])

  langford(k, num_sols)
  # benchmark()
