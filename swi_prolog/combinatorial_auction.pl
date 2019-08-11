/*

  Combinatorial auction in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        run(1).

go2 :-
        run(2).

go3 :-
        run(3).


run(Problem) :-
        problem(Problem,NumItems,NumBids,Packages,Bids),
        combinatorial_auction(NumItems,NumBids,Packages,Bids, X, Total),
        writeln(total=Total),
        writeln(x=X),
        findall(P,(between(1,NumBids,I),
                   element(I,X,1),
                   nth1(I,Packages,P)
                  ),
                Ps),
        writeln(packages=Ps),
        nl.

combinatorial_auction(NumItems,NumBids,Packages,Bids, X, Total) :-
        length(X,NumBids),
        X ins 0..1,

        %% Total #= sum([X[I]*Bids[I] : I in 1..NumBids]),
        scalar_product(Bids,X,#=,Total),

        %% ensure that each items is selected atmost once
        numlist(1,NumItems,Js),
        maplist(selected_atmost_once(X,NumBids,Packages),Js),
        
        labeling([max(Total)], X).

%% ensure that each items is selected atmost once
selected_atmost_once(X,NumBids,Packages,J) :-
        findall(I,
                (between(1,NumBids,I),
                 nth1(I,Packages,Package),
                 member(J,Package)
                ),
                Is),
        extract_from_indices(Is,X,Xs),
        sum(Xs,#=<,1).





%% The example cited above
problem(1,NumItems,NumBids,Packages,Bids) :-
        NumItems = 7,
        NumBids = 5,
        Packages =
        [[1,2,3,4],
         [2,3,6],
         [1,4,5],
         [2,7],
         [5,6]],
        Bids = [8,6,5,2,2].


%%
%% From Numberjack Tutorial, page 24 (slide 51/175)
%%
problem(2,NumItems,NumBids,Packages,Bids) :-
        NumItems = 4,
        NumBids = 5,
        Packages =
        [[1,2],
         [1,3],
         [2,4],
         [2,3,4],
         [1]],
        Bids = [8,6,5,2,2].

problem(3,NumItems,NumBids,Packages,Bids) :-
        NumItems = 10,
        NumBids = 5,
        Packages =
        [[2,5,6],
         [1,4,7,9,10],
         [2,4,8,10],
         [2,3,4,6,7],
         [1,5,8]],
        Bids = [8,6,5,2,2].
