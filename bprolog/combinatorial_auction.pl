/*

  Combinatorial auction problem in B-Prolog.

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




  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        num_items(NumItems),
        num_bids(NumBids),
        packages(Packages),
        bids(Bids),

        % decision variables
        length(X, NumBids),
        X :: 0..1,

        Total #= sum([X[I]*Bids[I] : I in 1..NumBids]),

        % ensure that each items is selected atmost once
        foreach(J in 1..NumItems,
                sum([(X[I]) : I in 1..NumBids,[PI],
                     (PI @= Packages[I], J in PI)
                     ]) #=< 1
               ),

        maxof(labeling(X),Total),

        writeln(total:Total),
        writeln(x:X),
        nl.

% The example cited above
num_items(7).
num_bids(5).
packages([[1,2,3,4],
          [2,3,6],
          [1,4,5],
          [2,7],
          [5,6]]).
bids([8,6,5,2,2]).


%%
%% From Numberjack Tutorial, page 24 (slide 51/175)
%%
% num_items(4).
% num_bids(5).
% packages([[1,2],
%           [1,3],
%           [2,4],
%           [2,3,4],
%           [1]]).
% bids([8,6,5,2,2]).
