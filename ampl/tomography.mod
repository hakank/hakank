/*
  Sat Apr 26 22:04:20 2008/hakank@bonetmail.com

  AMPL model of the discrete tomography problem.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param r; # number of rows
param c; # number of columns
param row_sums{1..r} integer >= 0;
param col_sums{1..c} integer >= 0;

var x{1..r, 1..c} binary; # decision variable


 # the rows
s.t. rows{i in 1..r}:
       row_sums[i] = sum{j in 1..c} x[i,j]
;

# the columns
s.t. cols{j in 1..c}: 
       col_sums[j] = sum{i in 1..r} x[i,j]
;


data;

#
# These examples is from the ECLiPSe program
# 
/*
param r := 11;
param c := 12;
param row_sums := 
        1 0,
        2 0,
        3 8, 
        4 2, 
        5 6, 
        6 4, 
        7 5, 
        8 3, 
        9 7, 
        10 0, 
        11 0;
param col_sums := 
        1 0, 
        2 0, 
        3 7, 
        4 1, 
        5 6, 
        6 3, 
        7 4, 
        8 5, 
        9 2, 
        10 7, 
        11 0, 
        12 0;
*/ 

/*
param r := 5;
param c := 13;
param row_sums = 
        1 10,
        2 4,
        3 8,
        4 5,
        5 6;
param col_sums = 
        1 5,
        2 3,
        3 4,
        4 0,
        5 5,
        6 0,
        7 5,
        8 2,
        9 2,
        10 0,
        11 1,
        12 5,
        13 1;
*/

/*
# This problem have slightly different solutions.
param r := 3;
param c := 11;
param row_sums = 
        1 11,
        2 5,
        3 4;
param col_sums = 
        1 3,
        2 2,
        3 3,
        4 1,
        5 1,
        6 1,
        7 1,
        8 2,
        9 3,
        10 2,
        11 1;
*/

# My own creation. :-)
# 12345678901234
#
# 00000000000000  1
# 00011000000000  2
# 00011000000000  3
# 00011000000000  4
# 00011000000000  5
# 00011000000000  6
# 00011111111000  7
# 00011111111000  8
# 00011000011000  9
# 00011000011000 10
# 00011000011000 11
# 00011000011000 12
# 00011000011000 13
# 00000000000000 14
#
param r := 14;
param c := 14;
param row_sums = 
        1 0,
        2 2,
        3 2, 
        4 2, 
        5 2,
        6 2,
        7 8,
        8 8,
        9 4,
        10 4,
        11 4,
        12 4,
        13 4,
        14 0;
param col_sums := 
        1 0,
        2 0,
        3 0,
        4 12,
        5 12,
        6 2,
        7 2,
        8 2,
        9 2,
        10 7,
        11 7,
        12 0,
        13 0,
        14 0;



option solver cplex;

solve;

display x;
