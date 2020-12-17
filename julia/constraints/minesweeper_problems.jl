
# Missing/unknown value
M = -1

#
# data
#
#  _ is coded as unknown
#  0..8: known number of neighbours
#

# The first 10 examples (0..9) are from Gecode/examples/minesweeper.cc
# http://www.gecode.org/gecode-doc-latest/minesweeper_8cc-source.html
# """
# The instances are taken from
#   http://www.janko.at/Raetsel/Minesweeper/index.htm
# """


all_minesweeper_problems = Dict(

# Problem from Gecode/examples/minesweeper.cc  problem 0
#
# Solution:
#  1 0 0 0 0 1
#  0 1 0 1 1 0
#  0 0 0 0 1 0
#  0 0 0 0 1 0
#  0 1 1 1 0 0
#  1 0 0 0 1 1
:0 =>
    [[M,M,2,M,3,M],
     [2,M,M,M,M,M],
     [M,M,2,4,M,3],
     [1,M,3,4,M,M],
     [M,M,M,M,M,3],
     [M,3,M,3,M,M]],


# Problem from Gecode/examples/minesweeper.cc  problem 1
:1 =>
    [[M,2,M,2,1,1,M,M],
     [M,M,4,M,2,M,M,2],
     [2,M,M,2,M,M,3,M],
     [2,M,2,2,M,3,M,3],
     [M,M,1,M,M,M,4,M],
     [1,M,M,M,2,M,M,3],
     [M,2,M,2,2,M,3,M],
     [1,M,1,M,M,1,M,1]],



# Problem from Gecode/examples/minesweeper.cc  problem 2
:2 =>
    [[1,M,M,2,M,2,M,2,M,M],
     [M,3,2,M,M,M,4,M,M,1],
     [M,M,M,1,3,M,M,M,4,M],
     [3,M,1,M,M,M,3,M,M,M],
     [M,2,1,M,1,M,M,3,M,2],
     [M,3,M,2,M,M,2,M,1,M],
     [2,M,M,3,2,M,M,2,M,M],
     [M,3,M,M,M,3,2,M,M,3],
     [M,M,3,M,3,3,M,M,M,M],
     [M,2,M,2,M,M,M,2,2,M]],


# Problem from Gecode/examples/minesweeper.cc  problem 3
:3 =>
    [[2,M,M,M,3,M,1,M],
     [M,5,M,4,M,M,M,1],
     [M,M,5,M,M,4,M,M],
     [2,M,M,M,4,M,5,M],
     [M,2,M,4,M,M,M,2],
     [M,M,5,M,M,4,M,M],
     [2,M,M,M,5,M,4,M],
     [M,3,M,3,M,M,M,2]],


# Problem from Gecode/examples/minesweeper.cc  problem 4
  :4 =>
    [[0,M,0,M,1,M,M,1,1,M],
     [1,M,2,M,2,M,2,2,M,M],
     [M,M,M,M,M,M,2,M,M,2],
     [M,2,3,M,1,1,M,M,M,M],
     [0,M,M,M,M,M,M,2,M,1],
     [M,M,M,2,2,M,1,M,M,M],
     [M,M,M,M,M,3,M,3,2,M],
     [M,5,M,2,M,M,M,3,M,1],
     [M,3,M,1,M,M,3,M,M,M],
     [M,2,M,M,M,1,2,M,M,0]],


# Problem from Gecode/examples/minesweeper.cc  problem 5
:5 =>
    [[M,2,1,M,2,M,2,M,M,M],
     [M,4,M,M,3,M,M,M,5,3],
     [M,M,M,4,M,4,4,M,M,3],
     [4,M,4,M,M,5,M,6,M,M],
     [M,M,4,5,M,M,M,M,5,4],
     [3,4,M,M,M,M,5,5,M,M],
     [M,M,4,M,4,M,M,5,M,5],
     [2,M,M,3,3,M,6,M,M,M],
     [3,6,M,M,M,3,M,M,4,M],
     [M,M,M,4,M,2,M,2,1,M]],



# Problem from Gecode/examples/minesweeper.cc  problem 6
:6 =>
    [[M,3,2,M,M,1,M,M],
     [M,M,M,M,1,M,M,3],
     [3,M,M,2,M,M,M,4],
     [M,5,M,M,M,5,M,M],
     [M,M,6,M,M,M,5,M],
     [3,M,M,M,5,M,M,4],
     [2,M,M,5,M,M,M,M],
     [M,M,2,M,M,3,4,M]],


# Problem from Gecode/examples/minesweeper.cc  problem 7
:7 =>
    [[M,1,M,M,M,M,M,3,M],
     [M,M,M,3,4,3,M,M,M],
     [2,4,4,M,M,M,4,4,3],
     [M,M,M,4,M,4,M,M,M],
     [M,4,M,4,M,3,M,6,M],
     [M,M,M,4,M,3,M,M,M],
     [1,2,3,M,M,M,1,3,3],
     [M,M,M,3,2,2,M,M,M],
     [M,2,M,M,M,M,M,3,M]],



# Problem from Gecode/examples/minesweeper.cc  problem 8
:8 =>
    [[M,M,M,M,M,M,M],
     [M,2,3,4,3,5,M],
     [M,1,M,M,M,3,M],
     [M,M,M,5,M,M,M],
     [M,1,M,M,M,3,M],
     [M,1,2,2,3,4,M],
     [M,M,M,M,M,M,M]],


# Problem from Gecode/examples/minesweeper.cc  problem 9
:9 =>
    [[2,M,M,M,2,M,M,M,2],
     [M,4,M,4,M,3,M,4,M],
     [M,M,4,M,M,M,1,M,M],
     [M,4,M,3,M,3,M,4,M],
     [2,M,M,M,M,M,M,M,2],
     [M,5,M,4,M,5,M,4,M],
     [M,M,3,M,M,M,3,M,M],
     [M,4,M,3,M,5,M,6,M],
     [2,M,M,M,1,M,M,M,2]],



# From "Some Minesweeper Configurations",page 2
:10 =>
    [[M,M,M,M,M,M],
      [M,2,2,2,2,M],
      [M,2,0,0,2,M],
      [M,2,0,0,2,M],
      [M,2,2,2,2,M],
      [M,M,M,M,M,M]],

# From "Some Minesweeper Configurations",page 3
# 4 solutions
:11 =>
    [[2,3,M,2,2,M,2,1],
      [M,M,4,M,M,4,M,2],
      [M,M,M,M,M,M,4,M],
      [M,5,M,6,M,M,M,2],
      [2,M,M,M,5,5,M,2],
      [1,3,4,M,M,M,4,M],
      [0,1,M,4,M,M,M,3],
      [0,1,2,M,2,3,M,2]],


# Richard Kaye: How Complicated is Minesweeper?
# http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
#
# A Wire,page 33
# 2 solutions
#
:12 =>
    [[M,0,0,0,0,0,0,0,0,0,0,0,0,M],
      [M,1,1,1,1,1,1,1,1,1,1,1,1,M],
      [M,M,1,M,M,1,M,M,1,M,M,1,M,M],
      [M,1,1,1,1,1,1,1,1,1,1,1,1,M],
      [M,0,0,0,0,0,0,0,0,0,0,0,0,M]],


# Richard Kaye: How Complicated is Minesweeper?
# http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
# A splitter,page 35
# Many solutions...
#
:13 =>
     [[M,M,M,0,M,M,M,0,M,M,M],
      [M,M,M,0,1,M,1,0,M,M,M],
      [M,M,M,0,1,M,1,0,M,M,M],
      [0,0,0,0,1,1,1,0,0,0,0],
      [M,1,1,1,1,M,1,1,1,1,M],
      [M,M,M,1,M,2,M,1,M,M,M],
      [M,1,1,1,1,M,1,1,1,1,M],
      [0,0,0,0,1,1,1,0,0,0,0],
      [M,M,M,0,1,M,1,0,M,M,M],
      [M,M,M,0,1,M,1,0,M,M,M],
      [M,M,M,0,M,M,M,0,M,M,M]],



# Oleg German,Evgeny Lakshtanov: "Minesweeper" without a computer
# http://arxiv.org/abs/0806.3480, page 4
:14 =>
     [[M,1,M,1,M,1],
      [2,M,2,M,1,M],
      [M,3,M,2,M,1],
      [1,M,3,M,2,M],
      [M,1,M,2,M,1]],


#
# From http://stephenlombardi.com/minesweeper/minesweeper.lisp
#
:15 =>
    [[0,0,0,0,1,M,M,M,M],
     [0,0,0,0,1,M,M,3,M],
     [0,0,1,1,2,M,M,M,M],
     [0,0,1,M,M,M,M,1,M],
     [0,0,1,2,M,3,M,M,M],
     [0,0,0,1,M,M,M,M,M],
     [0,0,1,2,2,1,1,1,1],
     [0,0,1,M,1,0,0,0,0],
     [0,0,1,M,1,0,0,0,0]]

)
