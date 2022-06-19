"""
  Frog circle in cpmpy.

  From https://www.cantorsparadise.com/flex-your-problem-solving-skills-with-this-viral-math-puzzle-cad27f6bffef
  '''
  Cards numbered 1 to 12 are arranged in a circle. A frog jumps on card 1, then jumps to the card 1 place 
  clockwise around the circle. Then, from card k, he jumps directly to the card k places clockwise around 
  the circle. The frog continues jumping in this way forever.

  Find an arrangement of cards that allows the frog to jump on every card.

  ...
  This problem appeared (with a slightly different context) in the 
  '2021 Pan-American Girls Mathematical Olympiad'
  https://artofproblemsolving.com/community/c2499895_2021_panamerican_girls_mathematical_olympiad
  '''

  (This is a port of my Picat model http://hakank.org/picat/frog_circle.pi )


  Solutions for n = 12
  --------------------
  For n = 12 there are 382 solutions. Here are some of them with nice symmetries:

    x      : [ 1 10  8  6  4  2 12 11  9  7  5  3]
    pos    : [ 0  1 11  2 10  3  9  4  8  5  7  6]
    visited: [ 1 10  3  8  5  6  7  4  9  2 11 12]
            1
        3       10
        5        8
        7        6
        9        4
       11        2
           12


    x      : [ 1  2  3  4  5  6 12  7  8  9 10 11]
    pos    : [ 0  1  3  7  2  5 11 10  8  4  9  6]
    visited: [ 1  2  4  7  3  6 11 10  8  5  9 12]
             1
       11        2
       10        3
        9        4
        8        5
        7        6
           12


  The first solution is the one that is proposed in the article cited above.
  It is also the solution of the 'algorithmic' approach in frog_circle_alg(n),
  see below for some more comments.


  Number of solutions for different n
  -----------------------------------
  First we can note that there is only solutions when n is even (or n is 1)
  (this is shown in a comment to the article).
  
  Here are the number of solutions for the first n:
  
    N   #sols
    -----------
    1        1
    2        1
    4        1
    6        1
    8        4
   10       43
   12      382
   14     7582
   16   195252
   18  6524755


  Visiting the numbers in order (1..n)
  ------------------------------------
  If we require that all the numbers should be visited in order (1..n), then
  there are only solutions for n=1,2,4,8,16,32,64,... (2**i | i in 1..)
  This additional constraint is activated by setting increasing_visits=True
  in the frog_circle() model.

  Here are some of these solutions :
  
  n: 4
  x      : [1 2 4 3]
  pos    : [0 1 3 2]
  visited: [1 2 3 4]
          1
      3        2
          4


  n: 8
  x      : [1 2 5 3 8 7 4 6]
  pos    : [0 1 3 6 2 7 5 4]
  visited: [1 2 3 4 5 6 7 8]
          1
      6        2
      4        5
      7        3
          8

  n: 16
  x      : [ 1  2 12  3  9  7  4 11 16 15  5 14  8 10 13  6]
  pos    : [ 0  1  3  6 10 15  5 12  4 13  7  2 14 11  9  8]
  visited: [ 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16]
          1
      6        2
     13       12
     10        3
      8        9
     14        7
      5        4
     15       11
         16

  As we see, the pattern is quite easy: Just place the value v v steps (modulo n)
  from the place of v-1.



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def print_clock(x):
    n = len(x)
    print(f"      {x[0]:3d}")
    for i in range(1,n // 2):
        print(f"  {x[n-i]:3d}      {x[i]:3d}")
    if n > 1:
        print(f"      {x[n // 2]:3d}")
    print()


#
# An imperative algorithmic version
#
# Solve Frog circle puzzle by algoritm ("imperative"):
#
# x = [1,10,8,6,4,2,12,11,9,7,5,3]
# visited: [1, 10, 3, 8, 5, 6, 7, 4, 9, 2, 11, 12]
#        1
#    3      10
#    5       8
#    7       6
#    9       4
#   11       2
#       12
#
# Note that the visiting order is alternating increasing odd number (starts with 1)
# and decreasing even number (starts with n-2).
#
# Also note that all "pairs" adds to 12+1 = 13 (i.e. n+1)
#   1 + 12 = 12 + 1 = 13
#   3 + 10 = 12 + 1 = 13
#   5 +  8 = 12 + 1 = 13
#   7 +  6 = 12 + 1 = 13
#   9 +  4 = 12 + 1 = 13
#  11 +  2 = 12 + 1 = 13
# 
def frog_circle_alg(n):
    print("n:",n)
    x         = [0 for i in range(n)]
    x[0]      = 1    # 1 is fixed
    x[n // 2] = n    # 1 + n div 2 is fixed  ("6 o'clock" on a traditional clock)
    for i in range(1,n // 2):
        x[n-i] = 2*i + 1       # n div 2+1..n: decreasing odd numbers 
        x[i] = n - x[n-i] + 1  # 2..n div 2  : decreasing even numbers
    print("x      :", x) 
    print("visited:",get_visited(x))   
    print_clock(x)


#
# Return the order of visited numbers
# (used by frog_circle_alg)
# 
def get_visited(x):
    n = len(x)
    xv = 1
    visited = [xv]
    posv = 0
    for _ in range(1,n):
        posv = (posv + xv) % n
        xv = x[posv]
        visited.append(xv)
    return visited

  
#
# This is the Constraint Programming approach
#
def frog_circle(n=12,print_solutions=True,increasing_visits=False):

    print("n:",n)
  
    # declare variables
    x       = intvar(1,n,shape=(n,),name="x")
    pos     = intvar(0,n-1,shape=(n,),name="pos")
    visited = intvar(1,n,shape=(n,),name="visited")

   
    # constraints
    model = Model([AllDifferent(x),
                   AllDifferent(pos),
                   AllDifferent(visited),
                   # Fix first position
                   x[0] == 1,
                   pos[0] == 0,
                   visited[0] == 1,
                   
                   ## For nice symmetric solutions (see solution 2 above)
                   # decreasing(x[1:(n // 2)]),
                   # decreasing(x[1+(n // 2):-1]),
                   ])

    # Require that we visits the number in order
    if increasing_visits:
        # model += [increasing(visited)]
        model += [visited == range(1,n+1)] # slightly faster

    # Loop through all the steps (except the initial)
    for i in range(1,n):
        # The next position is the previous position + the value of x in that position
        model += [pos[i] == (pos[i-1] + x[pos[i-1]]) % n]
        # The number that is visited in this step
        model += [visited[i] == x[pos[i]]]

    # Redundant constraints for fixing the value n (a little speed boost).
    # The last visited must be n since after that we are going in an infinite cycle...
    model += [visited[n-1] == n]
  
    # The position for n is n at "6 o'clock" 
    model += [pos[n-1] == n // 2]
    model += [x[n // 2 ] == n]

    # Print the solution
    def print_sol():
        if print_solutions:
            x_val = x.value()
            pos_val = pos.value()
            visited_val = visited.value()
            print("x      :",x_val)
            print("pos    :",pos_val)
            print("visited:",visited_val)
            print_clock(x_val)
            print()

    model.solveAll(display=print_sol)


# Print all solutions for n=12
n=12
print_solutions=True
frog_circle(n,print_solutions)

print("\nAlgoritmic approach (just one solution:")
frog_circle_alg(12)

# Just print the number of solutions
# print("N   #sols")
# print("---------")
# for n in range(1,20+1):
#     if n == 1 or n % 2 == 0: 
#         print(f"{n:2d}  {frog_circle(n,False):4d}")

## Algorithmic approach for some numbers
# for n in range(1,20+1):
#     if n == 1 or n % 2 == 0: 
#         frog_circle_alg(n)

## Visits the number in increasing order.
# increasing_visits = True
# for n in range(1,100):
#     if n == 1 or n % 2 == 0:
#         frog_circle(n,print_solutions,increasing_visits)
