/*

  Coin application in SICStus Prolog.
   
  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  Compare with the following model:
  * Comet: http://www.hakank.org/comet/coins3.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
       
        % the different coin values
        Variables = [1,2,5,10,25,50],

        length(Variables, N),

        length(Xs,N),
        domain(Xs,0,99),

        % total number of the coins used
        NumCoins in 0..99,
        sum(Xs,#=,NumCoins),
        
        % This is the "main loop":
        % Checks that all changes from 1 to 99 can be made.
        % To be honest I have forgotten exactly how
        % the ECLiPSe solution looked like.
        % Here is one way.
        ( for(J,1,99),
          param(Xs,Variables,N) do
              do_coins(J,Xs,Variables,N)
        ),

        append(Xs,[NumCoins],Vars),
        labeling([minimize(NumCoins)],Vars),

        write(num_coins:NumCoins),nl,
        write(Xs),nl,
        nl,
        fd_statistics.


%
% A related problem:
%
% Is there a set of coin denominations 
% which make it possible to change 
% into 1..99 with less than 8 coins?
% 
% Answer: Yes, there is. For example
% the following configuration:
%
% num_coins:7
% variables:[1,2,3,6,12,25,50]
% x:[1,1,1,1,1,1,1]
% 
% Though, it come with a cost of 
% using 7 different coin denominations.
%
%
% Also, we can see that there is
% a set with just 5 different 
% denominations for the 8 coin 
% change. This is done by changing the
% constraint to: 
%       NumCoins #=< 8,
%
% Result: 
% num_coins:8
% variables:[1,2,6,16,50]
% x:[1,2,2,2,1]
%
go2 :-

        % number of coin denominations
        N in 1..10,
        indomain(N),
        write(n:N),nl,

        % we assume that there are no coin
        % of a denomination larger than 50.
        length(Variables, N),
        domain(Variables,1,50),

        all_distinct(Variables),
        my_ordered(#=<, Variables),

        length(Xs,N),
        domain(Xs,0,8),

        % total number of the coins used
        % in the exchange
        sum(Xs,#=,NumCoins),
        
        % can we manage with less than 8 coins?
        NumCoins #< 8,

        % Note: we cannot use scalar_product/4 here
        % since Variables is not ground any more.
        ( for(J,1,99),
          param(Xs,Variables,N) do
              length(Tmp,N),
              domain(Tmp,0,99),
              ( foreach(X,Xs),
                foreach(T,Tmp),
                foreach(V,Variables),
                fromto(0,In,Out,TmpSum) do
                    T #=< X,
                    Out #= In + V*T
              ),
              TmpSum #= J
        ),

        % search
        append(Xs,Variables,Vars),
        labeling([ffc,bisect,up,minimize(NumCoins)],Vars),

        % output
        write(num_coins:NumCoins),nl,
        write(variables:Variables),nl,
        write(x:Xs),nl,
        nl,
        fd_statistics.



do_coins(J,Xs,Variables,N) :-
        length(Tmp,N),
        domain(Tmp,0,99),
        scalar_product(Variables,Tmp,#=,J),
        ( foreach(X,Xs),
          foreach(T,Tmp) do
              T #=< X
        ).
        

my_ordered(P,List) :-
        ( fromto(List, [This,Next | Rest], [Next|Rest],[_]),
          param(P)
        do
          call(P,This,Next)
        ).
