#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Blending problem in Z3
#
# From the OPL model blending.mod.
#
# z:  653.610000?
# metals:  ['0.046666?', '0', '0']
# raws:  ['0', '0']
# scraps:  ['17.416666?', '30.333333?']
# ingos:  [32]
# metal:  ['3.550000?', '24.849999?', '42.6']
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Optimize()

  #
  # data
  #
  NbMetals = 3
  NbRaw = 2
  NbScrap = 2
  NbIngo = 1
  Metals = list(range(NbMetals))
  Raws = list(range(NbRaw))
  Scraps = list(range(NbScrap))
  Ingos = list(range(NbIngo))

  CostMetal = [22, 10, 13]
  CostRaw = [6, 5]
  CostScrap = [7, 8]
  CostIngo = [9]
  Low = [0.05, 0.30, 0.60]
  Up = [0.10, 0.40, 0.80]
  PercRaw = [[0.20, 0.01], [0.05, 0], [0.05, 0.30]]
  PercScrap = [[0, 0.01], [0.60, 0], [0.40, 0.70]]
  # PercIngo = [[0.10], [0.45], [0.45]]
  # PercIngo = [[1/10], [9/20], [9/20]]
  PercIngo = [[Q(1,10)], [Q(9,20)], [Q(9,20)]]    
  Alloy = 71

  #
  # variables
  #
  p = [makeRealVar(sol, 'p[%i]' % i, 0.0, 1000.0) for i in Metals]
  r = [makeRealVar(sol, 'r[%i]' % i, 0.0, 1000.0) for i in Raws]
  s = [makeRealVar(sol, 's[%i]' % i, 0.0, 1000.0) for i in Scraps]
  metal = [makeRealVar(sol, 'metal[%i]'%j, Low[j] * Alloy, Up[j] * Alloy)  for j in Metals]
  ii = [makeIntVar(sol, 'ii[%i]' % i, 0, 1000) for i in Ingos]
  
  z = Real("z")

  #
  # constraints
  #
  sol.add(Sum(metal) == Alloy)
  sol.add(z ==
          Sum([CostMetal[i] * p[i] for i in Metals]) +
          Sum([CostRaw[i]   * r[i] for i in Raws]) +
          Sum([CostScrap[i] * s[i] for i in Scraps]) +
          Sum([CostIngo[i]  * ii[i] for i in Ingos]))

  for j in Metals:
    sol.add(
        metal[j] == p[j] +
        Sum([PercRaw[j][k]   * r[k] for k in Raws]) +
        Sum([PercScrap[j][k] * s[k] for k in Scraps]) +
        Sum([PercIngo[j][k]  * ii[k] for k in Ingos]))


  sol.minimize(z)

  
  found_z = 0
  found_ingos = None
  found_metals = None
  found_raws = None
  found_scraps = None
  found_metal = None  
  while sol.check() == sat:
    mod = sol.model()
    
    found_z = mod.eval(z).as_decimal(6)
    found_metals = [mod.eval(p[i]).as_decimal(6) for i in Metals]
    found_raws = [mod.eval(r[i]).as_decimal(6) for i in Raws]
    found_scraps = [mod.eval(s[i]).as_decimal(6) for i in Scraps]
    found_ingos = [mod.eval(ii[i]).as_long() for i in Ingos]
    found_metal = [mod.eval(metal[i]).as_decimal(6) for i in Metals]
    
    sol.add(z < mod.eval(z))
    
  print("z: ", found_z)
  print("metals: ", found_metals)
  print("raws: ", found_raws)
  print("scraps: ", found_scraps)        
  print("ingos: ", found_ingos)
  print("metal: ", found_metal)    

if __name__ == '__main__':
  main()
