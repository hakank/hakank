/*

  Coin application in AMPL+CP.

  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  Note: There are 4 optimal solutions of 8 coins:
    x: [1, 2, 1, 2, 1, 1]
    num_coins: 8
    ----------
    x: [1, 2, 2, 1, 1, 1]
    num_coins: 8
    ----------
    x: [2, 1, 1, 2, 1, 1]
    num_coins: 8
    ----------
    x: [2, 1, 2, 1, 1, 1]
    num_coins: 8


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n; # Number of coins

param variables{1..n};

var x{1..n} >= 0 <= 2 integer;
var num_coins >= 0 <= 10 integer;

# Temporary array for each coin value 1..99.
var tmp{1..99, 1..n} >= 0 <= 2 integer;

minimize obj: num_coins;

# constraints

#
# MiniZinc code (note the use of a temporary variable):
#
#   % Checks that all changes from 1 to 99 can be made
#   %
#   forall(j in 1..99) (
#     let {
#         array[1..n] of var 0..99: tmp} 
#      in
#      sum(i in 1..n) (tmp[i]*variables[i]) = j /\
#        forall(i in 1..n) (
#            tmp[i] <= x[i]
#       )
#   )

s.t. c1{c in 1..99}: c = sum{i in 1..n} (tmp[c, i]*variables[i]);

# s.t. c2{c in 1..99, i in 1..n}: tmp[c,i] <= x[i];
# This version is _much_ faster.
s.t. c2{i in 1..n}: x[i] = max{c in 1..99} tmp[c,i];
        
s.t. c3: num_coins = sum{i in 1..n} x[i];


data;

param n := 6;
param variables := 
        1 1
        2 2
        3 5 
        4 10
        5 25
        6 50;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

solve;

display x;
display num_coins;
# display tmp;
