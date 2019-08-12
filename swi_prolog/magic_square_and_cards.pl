/*

  Magic squares and cards in SWI Prolog

  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        N = 3,
        new_matrix(N,N,1..13,X),
        flatten(X,Vars),
        
        Ss #= 13*4,
        S in 0..Ss,            % the sum

        %% there are 4 cards of each value in a deck
        %% with max 4 occurrences
        findall(I,between(1,13,I),Is),
        maplist(atmost(4,Vars),Is),
   
        %% the standard magic square constraints (sans all_different)
        maplist(sums(S),X),
        transpose(X,XT),
        maplist(sums(S),XT),

        diagonal1_slice(X,Diagonal1),
        sum(Diagonal1,#=,S),

        diagonal2_slice(X,Diagonal2),
        sum(Diagonal2,#=,S),


        flatten([Vars,S],Vars2),
        labeling([ff,enum,max(S)], Vars2),
        
        writeln(s=S),
        maplist(writeln,X),

        nl.

%%
%% sum a row/column (for maplist/2)
%%
sums(Sum,L) :-
        sum(L,#=,Sum).

