"""
Send more money in any base problem in cpmpy.

Send More Money
   SEND
 + MORE
 ------
  MONEY

using any base.

Examples:
   Base 10 has one solution:
        {9, 5, 6, 7, 1, 0, 8, 2}
   Base 11 has three soltutions:
	{10, 5, 6, 8, 1, 0, 9, 2}
	{10, 6, 7, 8, 1, 0, 9, 3}
	{10, 7, 8, 6, 1, 0, 9, 2}
      

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *


def print_solution(x):
    print(x[0].value(),x[1].value(),x[2].value(),x[3].value(), " + ",
          x[4].value(),x[5].value(),x[6].value(),x[7].value(), " = ",
          x[8].value(),x[9].value(),x[10].value(),x[11].value(),x[12].value())
    

def send_more_money_any_base(base=10,num_sols=0,num_procs=1):
    print("\nbase: ", base)
    x = intvar(0,base-1,shape=8,name="x")
    s,e,n,d,m,o,r,y = x
    model = Model([
                              s*base**3 + e*base**2 + n*base + d +
                              m*base**3 + o*base**2 + r*base + e ==
                  m*base**4 + o*base**3 + n*base**2 + e*base + y,
                  s > 0,
                  m > 0,
                  AllDifferent((s,e,n,d,m,o,r,y))
                  ]
                  )

    xs = [s,e,n,d, 
          m,o,r,e, 
          m,o,n,e,y]

    def print_sol():
        print(x.value())

    # Use OR-tools CP-SAT for speeding up the program.
    ss = CPM_ortools(model)

    # Flags to experiment with        
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(display=print_sol)
    print("Nr solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())

    return num_solutions

b = 10
send_more_money_any_base(b)

start_val = 8
end_val = 30
num_sols_per_base = []
for b in range(start_val,end_val+1):
    num_sols_per_base += [send_more_money_any_base(b)]
print(f"\nNum sols per base ({start_val}..{end_val}):", num_sols_per_base)
