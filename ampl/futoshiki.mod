/*

  Futoshiki problem in AMPL+CP.

  http://en.wikipedia.org/wiki/Futoshiki
  http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param SIZE;
param lastdx; # index of last lt entry (0-based)

set NUMQD = 0..lastdx;
set RANGE = 1..SIZE;
set VALUES = 0..SIZE;

param values{RANGE,RANGE} default 0;
param lt{NUMQD,0..3};


var field{RANGE, RANGE} >= 1 <= SIZE  integer;


#
# constraints
#

# initial values
s.t. c0{row in RANGE, col in RANGE: values[row,col] > 0}:
   field[row,col] = values[row,col]
;
     

# rows
s.t. c1{row in RANGE}:
   alldiff{col in RANGE} field[row,col];

# columns
s.t. c2{col in RANGE}:
   alldiff{row in RANGE} field[row,col];

# ensure < constraints
s.t. c3{i in NUMQD}:
    field[ lt[i,0], lt[i,1] ] < field[ lt[i,2], lt[i,3]]
;


# data futoshiki1.dat;
data futoshiki2.dat;

# data;

## This is futoshiki2.dat
#
# # Example from Tailor/Essence model futoshiki.param/futoshiki.param
# #
# # Solution:
# # 5 1 3 2 4
# # 1 4 2 5 3
# # 2 3 1 4 5
# # 3 5 4 1 2
# # 4 2 5 3 1
# # 
# # Futoshiki instance, by Andras Salamon
# # specify the numbers in the grid
# param SIZE := 5;
# param values: 1 2 3 4 5 := 
#     1    . . 3 2 .
#     2    . . . . .
#     3    . . . . .
#     4    . . . . .
#     5    . . . . .
# ;
# # specify last index in array lt; lt[0] is first entry
# param lastdx = 10;
# # [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
# param lt: 0 1 2 3 :=
#   0      1 2 1 1 
#   1      1 4 1 5 
#   2      2 3 1 3 
#   3      3 3 2 3 
#   4      3 4 2 4 
#   5      2 5 3 5 
#   6      3 2 4 2 
#   7      4 4 4 3 
#   8      5 2 5 1 
#   9      5 4 5 3 
#  10      5 5 4 5
# ;

option solver gecode;
option gecode_options "icl=dom var_branching=regret_min_max val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "field:\n";
for{i in RANGE} {
  for{j in RANGE} {
     printf "%2d ", field[i,j];
  }
  printf "\n";
}

printf "\n";
