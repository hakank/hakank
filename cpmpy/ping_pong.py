"""
Ping pong puzzle in cpmpy.

This is a port (+ extension) of the z3 model from
From https://gist.github.com/MariaRigaki/b93f89cad49c7e7e35d94eed7abae5c3#file-ping_pong-py-L5
'''
Solution for the following problem:
3 friends (A, B and C) play ping-pong all day.
The winner always keeps playing. A plays 10 games, B 15, C 17. Who lost the 2nd game?
Problem by @eldracote https://twitter.com/eldracote/status/939614390571200514
'''

There are 420 possible solutions according to this model.
A lost the second game in all possible solutions,
in fact, A lost all the games he/she played.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


class solution_printer(ort.CpSolverSolutionCallback):
  """
  Solution printer.
  """
  def __init__(self, varmap, num_games,A,B,C,num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.num_games = num_games
    self.A_vars = (A)
    self.B_vars = (B)
    self.C_vars = (C)
    self.num_solutions=num_solutions
    self.lost_second_game = [] # Who lost second game?


  def on_solution_callback(self):
    self.solcount += 1

    # for wm in self.vars:
    #     for cpm_var in wm:
    #         cpm_var._value = self.Value(self.varmap[cpm_var])
    
    # For single arrays:
    for cpm_var in self.A_vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])
      
    for cpm_var in self.B_vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    for cpm_var in self.C_vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    print(f"#{self.solcount}:")      
    (A) = self.A_vars
    (B) = self.B_vars
    (C) = self.C_vars
    for i in range(self.num_games):
        print("Game {} A:{}, B:{}, C:{}".format(i+1, A[i].value(), B[i].value(), C[i].value()))
    print()
    # The looser of the game is the player with score == 1
    self.lost_second_game.append("ABC"[[A[1].value(),B[1].value(),C[1].value()].index(1)])

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


def ping_pong():

    num_games = 21
    dnp = 0 # do not play
    loss = 1
    win = 2

    num_played = [10,15,17]

    # Keep track of the players results (2 means win, 1 means loss, 0 is DNP)
    A = intvar(dnp,win,shape=num_games,name="a")
    B = intvar(dnp,win,shape=num_games,name="b")
    C = intvar(dnp,win,shape=num_games,name="c")    

    # Keep track whether each player played or not
    playedA = boolvar(shape=num_games,name="playedA")
    playedB = boolvar(shape=num_games,name="playedB")
    playedC = boolvar(shape=num_games,name="playedC")    

    model = Model()

    for i in range(num_games):
        model += (A[i] + B[i] + C[i] == 3) 
        if i < 20:
            model += ((A[i] == 1).implies(A[i+1] == 0))
            model += ((B[i] == 1).implies(B[i+1] == 0))
            model += ((C[i] == 1).implies(C[i+1] == 0))

        model += ( ((A[i] == 1) | (A[i] == 2) ).implies(playedA[i] == 1),
                   ((B[i] == 1) | (B[i] == 2) ).implies(playedB[i] == 1),
                   ((C[i] == 1) | (C[i] == 2) ).implies(playedC[i] == 1)
                   )
    model += (sum(playedA) == num_played[0],
              sum(playedB) == num_played[1],
              sum(playedC) == num_played[2])

    lost_second_game = []
    def print_sol():
      for i in range(num_games):
        print("Game {} A:{}, B:{}, C:{}".format(i+1, A[i].value(), B[i].value(), C[i].value()))
      print()
      # The looser of the game is the player with score == 1
      lost_second_game.append("ABC"[[A[1].value(),B[1].value(),C[1].value()].index(1)])


    ss = CPM_ortools(model)    
    # Flags to experiment with
    # ss.ort_solver.parameters.log_search_progress = True
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(display=print_sol)
    print("Nr solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()
    
    return lost_second_game

lost_second_game = ping_pong()
print("lost_second_game:","".join(lost_second_game))
