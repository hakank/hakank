#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Volsay problem in Z3
#
# From the OPL model volsay.mod
# """
# Consider a Belgian company Volsay, which specializes in producing ammoniac gas 
# (NH3) and ammonium chloride (NH4Cl). Volsay has at its disposal 50 units of 
# nitrogen (N), 180 units of hydrogen (H), and 40 units of chlorine (Cl). The company 
# makes a profit of 40 Euros for each sale of an ammoniac gas unit and 50 Euros 
# for each sale of an ammonium chloride unit. Volsay would like a production plan 
# maximizing its profits given its available stocks. 
# """
#
# Some different approaches.
#  
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from z3_utils_hakank import *

#
# Simple approach
#
def volsay1():

  sol = Optimize()
  
  Gas = Real("Gas")
  Chloride = Real("Chloride")
  obj = Real("obj")

  sol.add(Gas + Chloride <= 50)
  sol.add(3*Gas + 4*Chloride <= 180)
  sol.add(obj == 40*Gas + 50*Chloride)

  sol.maximize(obj)

  if sol.check() == sat:
    mod = sol.model()
    print "Gas:", mod.eval(Gas)
    print "Chloride:", mod.eval(Chloride)
    print "obj:", mod.eval(obj)

#
# With arrays
#
def volsay2():

  sol = Optimize()

  # data 
  Gas = 0
  Chloride = 1

  num_products = 2

  products = ['Gas', 'Chloride']

  # declare variables
  production = [makeRealVar(sol, 'production[%i]' % i, 0, 100000)
                for i in range(num_products)]
  obj = makeRealVar(sol,"obj",0, 100000)

  #
  # constraints
  #
  sol.add(production[Gas] + production[Chloride] <= 50.0)
  sol.add(3.0 * production[Gas] + 4.0 * production[Chloride] <= 180.0)

  # objective to minimize
  sol.add(obj == 40.0 * production[Gas] + 50.0 * production[Chloride])

  sol.maximize(obj)

  if sol.check() == sat:
    mod = sol.model()
    print "obj:", mod.eval(obj)
    for i in range(num_products):
      print products[i], '=', mod.eval(production[i])

#
# With added data
#
def volsay3():

  sol = Optimize()

  # data
  num_products = 2
  Gas, Chloride = range(num_products)
  products = ['Gas', 'Chloride']
  
  num_components = 3
  Nitrogen, Hydrogen, Chlorine = range(num_components)
  components = ['Nitrogen', 'Hydrogen', 'Chlorine']

  # Demands of Component per Product
  Demand = [[1.0, 3.0, 0.0],
            [1.0, 4.0, 1.0]] 

  Profit =  [30.0, 40.0]      # per product
  Stock = [50.0, 180.0, 40.0] # per component


  # declare variables
  Production = [makeRealVar(sol, 'Production[%i]' % i, 0, 100000)
                for i in range(num_products)]
  obj = makeRealVar(sol,"obj",0, 100000)

  #
  # constraints
  #
  for c in range(num_components):
    sol.add(Sum([Demand[p][c]* Production[p] for p in range(num_products)]) <= Stock[c])


  # objective to minimize
  sol.add(obj == Sum([Profit[p]*Production[p] for p in range(num_products)]))

  sol.maximize(obj)

  num_solutions = 0
  if sol.check() == sat:
    mod = sol.model()
    print "obj:", mod.eval(obj)
    for i in range(num_products):
      print products[i], '=', mod.eval(Production[i])


if __name__ == '__main__':
  print "volsay1:"
  volsay1()
  print "\nvolsay2:"
  volsay2()
  print "\nvolsay3 with added components:"
  volsay3()
