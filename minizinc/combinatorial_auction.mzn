% 
% Combinatorial auction problem in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Combinatorial_auction
% """
% A combinatorial auction is an auction in which bidders can place 
% bids on combinations of items, or "packages," rather than 
% just individual items. Simple combinatorial auctions have been 
% used for many years in estate auctions, where a common procedure 
% is to auction the individual items and then at the end to accept 
% bids for packages of items.
% """
%
%
% This simple example is from the lecture slides
% Constraint Satisfaction Problems, Constraint Optimization
% by Bernhard Nebel and Stefan Wölfl
% http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
% """
% In combinatorial auctions, bidders can give bids for set of items.
% The auctioneer [then] has to generate an optimial selection, e.g.
% one that maximizes revenue.
% 
% Definition
% The combinatorial auction problem  is specified as follows:
%   Given: A set of items Q = {q1,...,qn} and a set of bids
%          B = {b1,...,bm} such that each bid is bi = (Qi, ri),
%          where Qi (= Q and ri is a strictly positive real number.
%   Task: Find a subset of bids B'(= B such that any two bids in B'
%         do not share an item maximizing Sum(Qi,ri) (= Biri.
%
% ...
%
% Example Auction
%
% Consider the following auction:
%   b1 = {1,2,3,4}, r1 = 8
%   b2 = {2,3,6},   r2 = 6
%   b3 = {1,4,5},   r3 = 5
%   b4 = {2,8},     r4 = 2
%   b5 = {5,6},     r5 = 2
%
% What is the optimal assignment?
% """ 

%
% Note: This model is practically the same as the 
% set covering/set partition problem in 
%    http://www.hakank.org/minizinc/set_covering4b.mzn
% Problem description in
%    http://www.hakank.org/minizinc/set_covering4.mzn
%

%
% Solution:
%   total = 11;
%   x = [0, 1, 1, 0, 0]
%    
% i.e. the bids 2 and 3 are choosen: {2,3,6} and {1,4,5}
% to a cost of 6+5 = 11.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: num_items;
int: max_item;
set of int: items = 1..max_item;
int: num_bids;

array[1..num_bids] of set of items: packages;
array[1..num_bids] of int: bids;

% the assignments
array[1..num_bids] of var 0..1: x;
var int: total;

% solve maximize total;
solve :: int_search(x, first_fail, indomain_min, complete) maximize total;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   total = sum(i in 1..num_bids) ( x[i]*bids[i] )

   /\ % ensure that each items is selected atmost once
   forall(j in 1..num_items) (
       sum(i in 1..num_bids) (x[i]*bool2int(j in packages[i])) <= 1
   )
   % test
   % /\ total = 11

;

%
% data
%
num_items = 7;
num_bids = 5;
max_item = 7;
packages = [
   {1,2,3,4},
   {2,3,6},
   {1,4,5},
   {2,7},
   {5,6},
];
bids = [8,6,5,2,2];


% From Numberjack Tutorial, page 24 (slide 51/175)
% num_items = 4;
% num_bids = 5;
% max_item = 5;
% packages = [
%    {1,2},
%    {1,3},
%    {2,4},
%    {2,3,4},
%    {1}
% ];
% bids = [8,6,5,2,2];


output
[
  "x: " ++ show(x) ++ "\n" ++
  "total: " ++ show(total) 
];