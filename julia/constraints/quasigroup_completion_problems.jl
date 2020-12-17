
quasigroup_completion_problems = Dict(


#
# Example from Ruben Martins and InÃ¨s Lynce
# Breaking Local Symmetries in Quasigroup Completion Problems, page 3
# The solution is unique:
# 1 3 2 5 4
# 2 5 4 1 3
# 4 1 3 2 5
# 5 4 1 3 2
# 3 2 5 4 1
#
# Note: this is an array of arrays
#
:1 =>
    [[1, 0, 0, 0, 4],
    [0, 5, 0, 0, 0],
    [4, 0, 0, 2, 0],
    [0, 4, 0, 0, 0],
    [0, 0, 5, 0, 1]],


#
# Example from Gomes & Shmoys, page 3.
# Solution:
# 4 1 2 3
# 2 3 4 1
# 1 4 3 2
# 3 2 1 4
#
:2 =>
   [[0, 1, 2, 3],
    [2, 0, 4, 1],
    [1, 4, 0, 2],
    [3, 0, 1, 0]],

# Example from Gomes & Shmoys, page 7
# Two solutions.
#
:3 =>
    [[0, 1, 0, 0],
    [0, 0, 2, 0],
    [0, 3, 0, 0],
    [0, 0, 0, 4]],


#
# Example from Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/sec2.7.108.html
#
# 12 solutions.
#
:4 =>
   [[1, 0, 0, 0],
    [0, 0, 0, 3],
    [3, 0, 0, 0],
    [0, 0, 0, 1]],


#
# Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
# (n = 10]
# Pattern #1.
# There are 0many0 solutions to this problem.
#
:5 =>
   [[0,0,0,1,0,0,0,0,0,0],
    [0,0,1,0,0,0,0,0,0,0],
    [0,1,0,0,0,2,0,0,0,0],
    [1,0,0,0,2,0,0,0,0,0],
    [0,0,0,2,1,0,0,0,0,0],
    [0,0,2,0,0,1,0,0,0,0],
    [0,0,0,0,0,0,1,0,0,0],
    [0,0,0,0,0,0,0,1,0,2],
    [0,0,0,0,0,0,0,0,2,0],
    [0,0,0,0,0,0,0,2,0,0]],


#
# Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
# (n = 10]
# Pattern #2.
# There are 0many0 solutions to this problem.
#
:6 =>
   [[0,0,1,2,3,4,0,0,0,0],
    [0,1,2,3,0,0,4,0,0,0],
    [1,2,3,0,0,0,0,4,0,0],
    [2,3,0,0,0,0,0,0,4,0],
    [3,0,0,0,0,0,0,0,0,4],
    [5,6,0,0,0,0,0,0,0,0],
    [0,5,6,0,0,0,0,0,0,0],
    [0,0,5,6,0,0,0,0,0,0],
    [0,0,0,5,6,0,0,0,0,0],
    [0,0,0,0,5,6,0,0,0,0]],


#
# Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
# (n = 10]
# Pattern #3.
# Coding:
#    dark red   = 1
#    light blue = 2
#    dark blue  = 3
#    light red  = 4
#    brown      = 5
#    green      = 6
#    pink       = 7
#    grey       = 8
#    black      = 9
#    yellow     = 10
# There are 40944 solutions for this pattern.
#
:7 =>
   [[0, 0, 1, 5, 2, 6, 7, 8, 0, 0],
    [0, 1, 5, 2, 0, 0, 6, 7, 8, 0],
    [1, 5, 2, 0, 0, 0, 0, 6, 7, 8],
    [5, 2, 0, 0, 0, 0, 0, 0, 6, 7],
    [2, 0, 0, 0, 0, 0, 0, 0, 0, 6],
    [4,10, 0, 0, 0, 0, 0, 0, 3, 9],
    [0, 4,10, 0, 0, 0, 0, 3, 9, 0],
    [0, 0, 4,10, 0, 0, 3, 9, 0, 0],
    [0, 0, 0, 4,10, 3, 9, 0, 0, 0],
    [ 0, 0, 0, 0, 4,9, 0, 0, 0, 0]],


#
# Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
# (n = 10]
# Pattern #4.
#  dark red   = 1
#  light blue = 2
#  dark blue  = 3
#  light red  = 4
# Note: There are no solutions to this problem.
#
:8 =>
   [[1,0,0,0,0,0,0,0,0,0],
    [2,1,0,0,0,0,0,0,0,4],
    [3,2,1,0,0,0,0,0,4,0],
    [0,3,2,1,0,0,0,4,0,0],
    [0,0,3,2,1,0,4,0,0,0],
    [0,0,0,3,2,1,0,0,0,0],
    [0,0,0,0,3,2,1,0,0,0],
    [0,0,0,4,0,3,2,1,0,0],
    [0,0,4,0,0,0,3,2,1,0],
    [0,4,0,0,0,0,0,3,2,1]],


#
# Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
# (n = 10]
# Pattern #5
# Note: There are no solutions to this problem.
#
:9 =>
  [[0,0,0,0,0,0,0,0,0,1],
    [0,0,0,0,0,0,0,0,1,0],
    [0,0,0,0,0,0,0,1,0,0],
    [0,0,0,0,0,0,2,0,0,0],
    [0,0,0,0,0,1,0,0,0,0],
    [0,0,0,0,1,0,0,0,0,0],
    [0,0,0,1,0,0,0,0,0,0],
    [0,0,1,0,0,0,0,0,0,0],
    [0,1,0,0,0,0,0,0,0,0],
    [1,0,0,0,0,0,0,0,0,0]],

)
