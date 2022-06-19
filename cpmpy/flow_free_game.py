"""
Flow Free game in cpmpy.

https://www.bigduckgames.com/flowfree
'''
Flow Free® is a simple yet addictive puzzle game.

Connect matching colors with pipe to create a Flow®. Pair all colors, and cover the
entire board to solve each puzzle in Flow Free. But watch out, pipes will break if
they cross or overlap!
'''

This is a port of the z3 model (by commenter JohanC) in
https://stackoverflow.com/questions/67412516/how-to-solve-flow-game-using-google-or-tools

Note: This will show images (via matplotlib) for two instances. Close the window
of the first windo to see the next solution.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *
import matplotlib.pyplot as plt

def plot_solution(board,S):
    M = len(board)
    N = len(board[0])

    ax = plt.gca()
    colors = plt.cm.tab10.colors
    for i in range(M):
        for j in range(N):
            if board[i][j] != 0:
                ax.scatter(j, i, s=500, color=colors[board[i][j]])
            if S[i][j] != 0:
                for k in range(M):
                    for l in range(N):
                        if abs(k - i) + abs(l - j) == 1 and S[i][j] == S[k][l]:
                            ax.plot([j, l], [i, k], color=colors[S[i][j]], lw=15)
    ax.set_ylim(M - 0.5, -0.5)
    ax.set_xlim(-0.5, N - 0.5)
    ax.set_aspect('equal')
    ax.set_facecolor('black')
    ax.set_yticks([i + 0.5 for i in range(M - 1)], minor=True)
    ax.set_xticks([j + 0.5 for j in range(N - 1)], minor=True)
    ax.grid(b=True, which='minor', color='white')
    ax.set_xticks([])
    ax.set_yticks([])
    ax.tick_params(axis='both', which='both', length=0)
    plt.show()


def flow_free(board):
  M = len(board)
  N = len(board[0])
  B = intvar(1,10,shape=(N,M), name="B")

  model = Model()

  for i in range(M):
    for j in range(N):
      same_neighs_ij = sum([B[i][j] == B[k][l] 
                            for k in range(M) for l in range(N) if abs(k - i) + abs(l - j) == 1])
      if board[i][j] != 0:
        model += [B[i,j] == board[i][j]]
        model += [same_neighs_ij == 1]
      else:
        model += [(same_neighs_ij == 2) | (B[i][j] == 0)]

  def print_sol():
    print(B.value())
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())
  plot_solution(board,B.value())
  

board1 = [[1, 0, 0, 2, 3],
          [0, 0, 0, 4, 0],
          [0, 0, 4, 0, 0],
          [0, 2, 3, 0, 5],
          [0, 1, 5, 0, 0]]

# Another instance
board2 = [[0, 1, 2, 0, 0, 0, 0],
          [1, 3, 4, 0, 3, 5, 0],
          [0, 0, 0, 0, 0, 0, 0],
          [0, 2, 0, 4, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0],
          [0, 5, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0]]


print("Board1. Close the window to see the next instance.")
flow_free(board1)

print("\nBoard2")
flow_free(board2)
