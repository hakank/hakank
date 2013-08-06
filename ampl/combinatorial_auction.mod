/*

  Combinatorial auction in AMPL+CP.

  http://en.wikipedia.org/wiki/Combinatorial_auction
  """
  A combinatorial auction is an auction in which bidders can place 
  bids on combinations of items, or "packages," rather than 
  just individual items. Simple combinatorial auctions have been 
  used for many years in estate auctions, where a common procedure 
  is to auction the individual items and then at the end to accept 
  bids for packages of items.
  """

  This simple example is from the lecture slides
  Constraint Satisfaction Problems, Constraint Optimization
  by Bernhard Nebel and Stefan Wölfl
  http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
  """
  In combinatorial auctions, bidders can give bids for set of items.
  The auctioneer [then] has to generate an optimial selection, e.g.
  one that maximizes revenue.
  
  Definition
  The combinatorial auction problem  is specified as follows:
    Given: A set of items Q = {q1,...,qn} and a set of bids
           B = {b1,...,bm} such that each bid is bi = (Qi, ri),
           where Qi (= Q and ri is a strictly positive real number.
    Task: Find a subset of bids B'(= B such that any two bids in B'
          do not share an item maximizing Sum(Qi,ri) (= Biri.

  ...

  Example Auction

  Consider the following auction:
    b1 = {1,2,3,4}, r1 = 8
    b2 = {2,3,6},   r2 = 6
    b3 = {1,4,5},   r3 = 5
    b4 = {2,8},     r4 = 2
    b5 = {5,6},     r5 = 2

  What is the optimal assignment?
  """ 



  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param num_items;
param max_item;
param num_bids;
param packages{1..num_bids, 1..num_items} binary default 0;
param bids{1..num_bids};


# decision variables
var x{1..num_bids} binary;
var total >= 0 <= 100 integer;

maximize obj: total;

#
# constraints
#
s.t. c1: total = sum{i in 1..num_bids} x[i]*bids[i];

# ensure that each items is selected atmost once
s.t. c2{j in 1..num_items}:
       sum{i in 1..num_bids} (x[i]*packages[i,j]) <= 1
;

# data combinatorial_auction1.dat;
data combinatorial_auction2.dat;

# data;

# param num_items := 7;
# param num_bids := 5;
# param max_item = 7;

# # packages = [
# #    {1,2,3,4},
# #    {2,3,6},
# #    {1,4,5},
# #    {2,7},
# #    {5,6},
# # ];
# param packages: 1 2 3 4 5 6 7 :=
#  #  1 2 3 4 5 6 7
#  1  1 1 1 1 . . . 
#  2  . 1 1 . . 1 . 
#  3  1 . . 1 1 . . 
#  4  . 1 . . . . 1
#  5  . . . . 1 1 . 
# ;

# param bids := 
#   1 8
#   2 6
#   3 5 
#   4 2
#   5 2
# ;



# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;
display total;

