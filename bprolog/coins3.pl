/*

  Coin application in in B-Prolog.

  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  This is a port of my SICStus Prolog model:
  http://hakank.org/sicstus/coins3.pl

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
       
        % the different coin values
        Variables = [1,2,5,10,25,50],

        length(Variables, N),

        length(Xs,N),
        Xs :: 0..99,

        % total number of the coins used
        NumCoins in 0..99,
        NumCoins #= sum(Xs),
        
        % This is the "main loop":
        % Checks that all changes from 1 to 99 can be made.
        % To be honest I have forgotten exactly how
        % the ECLiPSe solution looked like.
        % Here is one way.
        foreach(J in 1..99,
              do_coins(J,Xs,Variables,N)
        ),

        minof(labeling(Xs), NumCoins),

        write(num_coins:NumCoins),nl,
        write(Xs),nl,
        nl.


do_coins(J,Xs,Variables,N) :-
        length(Tmp,N),
        Tmp :: 0..99,
        scalar_product(Variables,Tmp,#=,J),
        foreach(I in 1..N, Tmp[I] #=< Xs[I]).
        

increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).


%
% A related problem:
%
% Is there a set of coin denominations which make it possible to change 
% into 1..99 with less than 8 coins?
% 
% Answer: Yes, there is. For example the following configuration:
%
% num_coins:7
% variables:[1,2,3,6,12,25,50]
% x:[1,1,1,1,1,1,1]
% 
% Though, it come with a cost of using 7 different coin denominations.
%
% Also, we can see that there is a set with just 5 different 
% denominations for the 8 coin change. However, it requires that 
% more than one coin of some of the denominations
%
% This is done by comment this constraint:
%       NumCoins #=< 8,
%
% Result: 
%   num_coins:8
%   variables:[1,2,4,11,33]
%   x:[1,1,2,2,2]
%
go2 :-

        % number of coin denominations
        % Note: It increases with each failure to
        %       solve the instance.
        N in 1..10,
        indomain(N),
        write(n:N),nl,

        % we assume that there are no coin
        % of a denomination larger than 50.
        length(Variables, N),
        Variables :: 1..50,

        alldifferent(Variables),
        increasing(Variables),

        length(Xs,N),
        Xs :: 1..2,

        % total number of the coins used
        % in the exchange
        NumCoins #= sum(Xs),
        
        % can we manage with less than 8 coins?
        NumCoins #< 8,
        foreach(J in 1..99, do_coins(J,Xs,Variables,N)),

        % search
        term_variables([Xs,Variables],Vars),
        minof(labeling([ff],Vars),NumCoins),

        % output
        write(num_coins:NumCoins), nl,
        write(variables:Variables), nl,
        write(x:Xs), nl, nl.
