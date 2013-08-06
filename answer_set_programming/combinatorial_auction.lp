% 
% Combinatorial auction problem in ASP.
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
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const num_bids = 5.

%
% package(bid, items)
%
package(1, 1;2;3;4).
package(2, 2;3;6).
package(3, 1;4;5).
package(4, 2;8).
package(5, 5;6).

% This is from Numberjack Tutorial, page 24 (slide 51/175)
% package(1, 1;2).
% package(2, 1;3).
% package(3, 2;4).
% package(4, 2;3;4).
% package(5, 1).

cost(1,8).
cost(2,6).
cost(3,5).
cost(4,2).
cost(5,2).

% domains
bids(1..num_bids).

% all items
items(Item) :- package(Bid, Item), bids(Bid).

%
% set partition: there may be no overlap of items of the selected bids.
% we may take 0..1 items of each item
%
{ selected(Bid) : bids(Bid) : package(Bid, Item) } 1 :- items(Item).

% which items are selected
selected_items(Item) :- selected(Bid), bids(Bid), package(Bid, Item).

% the total cost
total_cost(T) :- T = #sum [selected(Bid) : cost(Bid, Cost) = Cost].

%
% minimize the the total cost of the selected bid
%
#maximize [ selected(Bid) : cost(Bid, Cost) : bids(Bid) = Cost].



#hide.
#show selected(Bid).
#show selected_items(Bid).
#show total_cost(Total).