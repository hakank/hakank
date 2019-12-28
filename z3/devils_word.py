#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# "Devil's Word" in Z3
#
# i.e. addition/subtraction of an array of numbers to give a specific total (e.g. 666)
#
# Compare to my CGI program "Devil's Word"
#  http://www.hakank.org/data_snooping/666.cgi
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

def devils_word(total, arr):
  sol = Solver()

  n = len(arr)
  max_val = max(arr)

  # variables
  plus = makeIntVector(sol,"plus",n,0,1)   # is the number arr[i] to be added
  minus = makeIntVector(sol,"minus",n,0,1) # or is it to be subtracted?

  # array with the number with correct sign
  result = makeIntVector(sol,"result", n, -max_val,max_val)

  # number of minus entries (perhaps to be minimized)
  num_minus = makeIntVar(sol,"num_minus",0,sum(arr)) 

  # constraints

  # calculate the sum of the numbers in arr
  # sol.add(total == Sum([arr[i]*plus[i] + (-arr[i])*minus[i] for i in range(n)]))
  sol.add(total == Sum([result[i] for i in range(n)]))
          
  for i in range(n):
    # either plus or minus
    sol.add(plus[i] + minus[i] == 1) 
    # calculate the result array
    sol.add(result[i] == arr[i]*plus[i] + (-arr[i])*minus[i])

  sol.add(num_minus == Sum([If(minus[i] == 1,1,0) for i in range(n)]))

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("total:", total)
    print([mod.eval(result[i]) for i in range(n)])
    print("plus :", [mod.eval(plus[i]) for i in range(n)])
    print("minus:", [mod.eval(minus[i]) for i in range(n)])
    print("num_minus:", mod.eval(num_minus))
    print()
    getDifferentSolution(sol,mod,result)
    # getLessSolution(sol,mod,num_minus)

  print("num_solutions:", num_solutions)
 

# My name ("Håkan Kjellerstrand") in ASCII numbers.
# Cf http://www.hakank.org/data_snooping/666.cgi?name=H#E5kan+Kjellerstrand&submit=ok
# which gives the solution:
# +72+229+107+97+110+32+75-106+101+108-108+101-114-115-116-114+97+110+100 = 666
#
# There are 288 different solutions...
#
total = 666
s = "Håkan Kjellerstrand"
print("s:", s)
arr = [ord(c) for c in s]
# arr = [72, 229, 107, 97, 110, 32, 75, 106, 101, 108, 108, 101, 114, 115, 116, 114, 97, 110, 100]

devils_word(total,arr)
