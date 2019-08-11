/*

  Two coin applications in SWI Prolog

  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  There are 4 optimal solutions (8 coins):
    x=[1,2,1,2,1,1]
    x=[1,2,2,1,1,1]
    x=[2,1,1,2,1,1]
    x=[2,1,2,1,1,1]


  The other application (go2/0) is to create the denominations
  which minimize the number of coins used in this way.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% First coin problem
%
go :-
       
   % the different coin values
   Variables = [1,2,5,10,25,50],

   length(Variables,N),

   length(Xs,N),
   Xs ins 0..99,

   % total number of the coins used
   NumCoins in 0..99,
   sum(Xs,#=,NumCoins),
   
   % This is the "main loop":
   % Checks that all changes from 1 to 99 can be made.
   % To be honest I have forgotten exactly how
   % the ECLiPSe solution looked like.
   % Here is one way.
   coins1_constraint(Xs,Variables,N),
   
   labeling([min(NumCoins)], Xs),

   writeln(num_coins=NumCoins),
   writeln(x=Xs),
   nl.


coins1_constraint(Xs,Variables,N) :-
        numlist(1,99,Js),
        maplist(do_coins(Xs,N,Variables),Js).

do_coins(Xs,N,Variables,J) :-
        length(Tmp,N),
        Tmp ins 0..99,
        %% scalar_product(Variables,Tmp,#=,J),
        scalar_product2(Variables,Tmp,J),
        maplist(tmp_lesseq_than_x,Tmp,Xs).
   
tmp_lesseq_than_x(Tmp,X) :-
        Tmp #=< X.
                
%
% A related problem:
%
% We can see that there is a set with just 5 different 
% denominations for the 8 coin change. However, it requires that 
% more than one coin of some of the denominations
%
% Result: 
%   num_coins:8
%   variables:[1,2,4,11,33]
%   x:[1,1,2,2,2]
%                               %
% 
% Is there a set of coin denominations which make it possible to change 
% into 1..99 with less than 8 coins?
% 
% Answer: Yes, there is. For example the following configuration:
%                                %
% This is done by this constraint:
%       NumCoins #=< 8,
%
% num_coins:7
% variables:[1,2,3,6,12,25,50]
% x:[1,1,1,1,1,1,1]
% 
% Though, it come with a cost of using 7 different coin denominations.
%
go2 :-

        %% Number of coin denominations
        N in 1..10,
        indomain(N),
        writeln(n=N),

        %% we assume that there are no coin of a denomination larger than 50.
        length(Variables,N),
        Variables ins 1..50,

        all_different(Variables),
        increasing_strict(Variables),

        length(Xs,N),
        Xs ins 1..2, % we'll use either one or two coins

        %% total number of the coins used
        %% in the exchange
        %% optimize over total number of coins
        sum(Xs,#=,NumCoins),

        %% Check the combinations
        numlist(1,99,Js),
        maplist(do_coins(Xs,N,Variables),Js),
  
        %% Now, can we manage with less than 8 coins?
        NumCoins #< 8,

        %% search
        flatten([Variables,Xs],Vars),
        labeling([ff,bisect,min(NumCoins)],Vars),
        
        writeln(num_coins=NumCoins), 
        writeln(variables=Variables),
        writeln(x=Xs), nl, nl.
