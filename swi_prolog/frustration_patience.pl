/*

  Frustration patience in SWI Prolog

  From https://community.wolfram.com/groups/-/m/t/1609558
  """
  Frustration solitaire is a game that has roots stemming from the early 
  1700's. The rules of the game are simple: a dealer calls out the ranks 
  of cards in order Ace, Two, Three, . . . and so on. At the same time the 
  dealer draws a card from a sufficiently well shuffled deck. If the rank 
  of the card drawn matches the rank of the card the dealer says you lose 
  the game. 
  ...
  Now we shouldn't feel too bad about losing. The name "frustration" 
  solitaire stems from the fact that the percentage of winning is actually 
  very low. In 2009, Doyle et. al. found out that the percentage of winning 
  a game of frustration solitaire is approximately 1.62%.
  ...
  In our 100,000 games of frustration solitaire we won 1.61% of the time, 
  hence the title of "frustration" solitaire is very well deserved. 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Generate one round of Frustration Patience.
%% Show everything.
go :- 
        gen_deck(Deck),
        random_permutation(Deck,Shuffled),
        writeln(Shuffled),
        maplist(check_hit,Deck,Shuffled,Cs),
        writeln(cs=Cs),
        sum_list(Cs,Counts),
        (
         Counts #> 0
        ->
         writeln("You loose!")
        ;
         writeln("You win")
        )
        ,
        nl.

%%
%% Show just counts.
%%
go2 :-
        gen_deck(Deck),
        check2(Deck,Counts,Outcome),
        writeln(counts=Counts),
        writeln(outcome=Outcome),
        nl.


%%
%% Now we simulate 100_000 rounds.
%%
%% Here we compare the full list och Deck and Shuffled and
%% after that compare the outcome.
%%
%% Slightly slower than go4:
%%
%% n=100000
%% numWon=1551
%% pct=1.551
%%
%% % 53,101,540 inferences, 2.875 CPU in 2.875 seconds (100% CPU, 18466905 Lips)
%% 
go3 :-
        N = 100_000,
        writeln(n=N),
        gen_deck(Deck),        
        findall(Outcome,
                (between(1,N,_),
                 check3(Deck,Outcome)
                ),
                Games),
        sum_list(Games,NumWon),
        writeln(numWon=NumWon),
        Pct is 100*NumWon / N,
        writeln(pct=Pct),
        nl.

%%
%% End the comparison of Deck and Shuffled as soon as possible,
%% i.e. as soon as when we got a loss. So we coun the number of
%% losses, not the wins.
%%
%% It's faster than go3/0 by a second.
%%
%% For N=100_000 it takes about 1.7s
%%
%% n=100000
%% numLoss=98471
%% won_pct=1.529
%% 
%% % 24,474,050 inferences, 1.766 CPU in 1.766 seconds (100% CPU, 13856915 Lips)
%%
%% n=1000000
%% numLoss=983873
%% won_pct=1.6127
%%
%% 244,773,976 inferences, 18.065 CPU in 18.066 seconds (100% CPU, 13549273 Lips)
%%
go4 :-
        gen_deck(Deck),
        N = 100_000,
        writeln(n=N),
        findall(Loss,
                (between(1,N,_),
                 random_permutation(Deck,Shuffled),
                 check4(Deck,Shuffled,0,Loss)
                ),
                Games),
        sum_list(Games,NumLoss),
        writeln(numLoss=NumLoss),
        WonPct is 100*(N-NumLoss) / N,
        writeln(won_pct=WonPct),
        nl.


check2(Deck, Counts, Outcome) :-
        random_permutation(Deck,Shuffled),
        maplist(check_hit,Deck,Shuffled,Cs),
        sum_list(Cs,Counts),
        (
         Counts > 0
        ->
         Outcome = 0
        ;
         Outcome = 1
        ).

%% As check2/3 but w/o returning Counts
check3(Deck, Outcome) :-
        random_permutation(Deck,Shuffled),
        maplist(check_hit,Deck,Shuffled,Cs),
        sum_list(Cs,Counts),
        (
         Counts > 0
        ->
         Outcome = 0
        ;
         Outcome = 1
        ).

%%
%% We end as soon as we get a loss. Faster.
%%
check4([],[],Outcome,Outcome).
check4([D|_Deck],[D|_Shuffled],Outcome0,Outcome) :-
        %% We loose :-(
        Outcome1 is Outcome0 + 1,
        check4([],[],Outcome1,Outcome).
check4([D|Deck],[S|Shuffled],Outcome0,Outcome) :-
        %% We can still win!
        D \= S,
        check4(Deck,Shuffled,Outcome0,Outcome).


check_hit(I,D, C) :-
        (I =:= D
        ->
         C = 1
        ;
         C = 0
         ).
% I don't like the cut!
/*
  check_hit(I,I,1) :- !.
  check_hit(I,D,0) :-
        I \= D.
*/

%%
%% Generate a 52 card deck
%%
gen_deck(Deck) :-
        findall(X, (between(0,51,N), 1+(N mod 13) #= X),Deck).
