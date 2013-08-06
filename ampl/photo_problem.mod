/*

  Photo problem in AMPL+CP.

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for taking 
  a photo. Some of them have preferences next to whom they want to stand:

     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.

  Obviously, it is impossible to satisfy all preferences. Can you find an alignment 
  that maximizes the number of satisfied preferences?
  """

  Oz solution: 
    6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]
  
  There are 8 solutions:
    positions = [3, 1, 6, 5, 2, 4, 7]
    positions = [3, 1, 7, 5, 2, 4, 6]
    positions = [3, 2, 6, 5, 1, 4, 7]
    positions = [3, 2, 7, 5, 1, 4, 6]
    positions = [5, 6, 1, 3, 7, 4, 2]  (the Oz solution.)
    positions = [5, 6, 2, 3, 7, 4, 1]
    positions = [5, 7, 1, 3, 6, 4, 2]
    positions = [5, 7, 2, 3, 6, 4, 1]

  Also, compare with the AMPL example:   
     http://www.ampl.com/NEW/LOGIC/EXAMPLES/photo.mod


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

set PERSONS;
param n; # number of persons
param preferences{PERSONS, PERSONS};

var positions{PERSONS} >= 1 <= n integer; 
var z >= 0 <= n*n integer;

maximize obj: z;

#
# constraints
#
s.t. c1: alldiff{i in PERSONS} positions[i];

s.t. c2: z = count{i in PERSONS, j in PERSONS: preferences[i,j] = 1} 
                   (abs(positions[i]-positions[j]) = 1);

## Alternative encoding:
# s.t. c2: exactly z {i in PERSONS, j in PERSONS: preferences[i,j] = 1} 
#                    (abs(positions[i]-positions[j]) = 1);

# data photo_problem1.dat;
data photo_problem2.dat;
# data photo_problem3.dat;


# data;

## This is the problem from Oz (show above).
# param n := 7;
# set PERSONS := 1 2 3 4 5 6 7;
# param preferences: 
# #  B C D F G M P
#    1 2 3 4 5 6 7 := 
# 1  0 0 0 0 1 1 0  # Betty  1
# 2  1 0 0 0 1 0 0  # Chris  2
# 3  0 0 0 0 0 0 0  # Donald 3
# 4  0 0 1 0 0 1 0  # Fred   4
# 5  0 0 0 0 0 0 0  # Gary   5
# 6  0 0 0 0 0 0 0  # Mary   6
# 7  0 0 1 1 0 0 0  # Paul   7
# ;

option solver gecode;
# option gecode_options "var_branching=regret_min_max val_branching=min outlev=1 outfreq=1 timelimit=30";
# option gecode_options "var_branching=max_min val_branching=max outlev=1 outfreq=1 timelimit=30";
# This seems to be the best variant of labeling.
option gecode_options "var_branching=max_min val_branching=split_max outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=2 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "z: %d\n", z;
printf "positions: ";
for{i in PERSONS} {
  printf "%2d ", positions[i];
}
printf "\n";
