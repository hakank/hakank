
#
# Data
#

survo_puzzle_problems  = Dict(

# http://en.wikipedia.org/wiki/Survo_Puzzle, first example
#
# Solution:
#  12 6 2 10
#  8 1 5 4
#  7 9 3 11
#
:1 => Dict(
        :row_sums => [30,18,30],
        :col_sums => [27,16,10,25],
        :problem => [[0, 6, 0, 0],
                   [8, 0, 0, 0],
                   [0, 0, 3, 0]]
      ),


# http://en.wikipedia.org/wiki/Survo_Puzzle, second example
# difficulty 0
:2  => Dict(
        :row_sums => [9, 12],       # :row_sums
        :col_sums => [9, 7, 5],     # :col_sums
        :problem => [[0, 0, 3],  # :problem
                   [0, 6, 0]]
       ),


# http://en.wikipedia.org/wiki/Survo_Puzzle, third example
# difficulty 150 ("open puzzle", i.e. no hints]
# It's an unique solution.
# (817 propagations with Gecode/fz, and 33 failures, 88 commits]
# r = 3;
# c = 4;
# :row_sums = [24,15,39];
# :col_sums = [21,10,18,29];
# matrix = array2d(1..r, 1..c,
#   [
#     0, 0, 0, 0,
#     0, 0, 0, 0,
#     0, 0, 0, 0
#   ]];
# Note: this version has no hints
:3  => Dict(
        :row_sums => [24,15,39],      # :row_sums
        :col_sums => [21,10,18,29],   # :col_sums
        :problem => [[0, 0, 0, 0], # :problem
                   [0, 0, 0, 0],
                   [0, 0, 0, 0]]
        ),



# same as above but with hints: difficulty 0
# (15 propagations with Gecode/fz, no failures, no commits]
# matrix = array2d(1..r, 1..c,
#    [
#      7, 0, 5, 0,
#      0, 1, 0, 8,
#      0, 0, 11, 0
#    ]];
:4  => Dict(
       :row_sums => [24,15,39],      # :row_sums
       :col_sums => [21,10,18,29],   # :col_sums
       :problem => [[7, 0, 5, 0], # :problem
                  [0, 1, 0, 8],
                  [0, 0, 11, 0]]
       ),


# http://www.survo.fi/puzzles/280708.txt, third puzzle
# Survo puzzle 128/2008 (1700] #364-35846
#
#    A  B  C  D  E  F
# 1  *  *  *  *  *  * 30
# 2  *  * 18  *  *  * 86
# 3  *  *  *  *  *  * 55
#   22 11 42 32 27 37
:5  => Dict(
       :row_sums => [30, 86, 55],
       :col_sums => [22, 11, 42, 32, 27, 37],
       :problem => [[0, 0,  0, 0, 0, 0],
                  [0, 0, 18, 0, 0, 0],
                  [0, 0,  0, 0, 0, 0]]
        ),
#
# http://en.wikipedia.org/wiki/Survo_Puzzle, under "Swapping method"
# (open puzzle]
#
:6  => Dict(
       :row_sums => [51,36,32,17],
       :col_sums => [51,42,26,17],
       :problem => [[0, 0, 0, 0],
                  [0, 0, 0, 0],
                  [0, 0, 0, 0],
                  [0, 0, 0, 0]]
      ),
)
