/*

  Euler problem 31 in SICStus Prolog

  Problem 31
  """
  In England the currency is made up of pound, £, and pence, p, and 
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             % euler31a,
             % euler31b,
             % euler31c,
             euler31d
            ],
        run_problems(L).

%%
%% 0.085s
%%
euler31a :-
        Coins = [200,100,50,20,10,5,2,1],
        findall(X,coins_a(Coins,X),L),
        length(L,Len),
        writeln(Len).


%%
%% 0.067s
%%
euler31b :-
        Coins = [200,100,50,20,10,5,2,1],
        findall(X,coins_b(Coins,X),L),
        length(L,Len),
        writeln(Len).

%%
%% 0.067s
%%
euler31c :-
        findall(X,coins_c(X),L),
        length(L,Len),
        writeln(Len).

%%
%% 0.016s
%% 
euler31d :-
        Coins = [200,100,50,20,10,5,2,1],
        coins_d(Coins,200,1,T),
        writeln(T).

coins_a(Coins,X) :-
        length(Coins,Len),
        length(X,Len),
        domain(X,0,200),
        scalar_product(Coins,X,#=,200),
        labeling([ff,enum],X).

%% stricter domain
coins_b(Coins,X) :-
        length(Coins,Len),
        length(X,Len),
        max_list(Coins,MaxC),
        maplist(set_domain(MaxC),X,Coins),
        scalar_product(Coins,X,#=,200),
        labeling([ff,enum],X).

%% Restrict the domain of this X
set_domain(MaxC,X,C) :-
        MaxD #= MaxC div C,
        X in 0..MaxD.

coins_c(X) :-
        X = [A,B,C,D,E,F,G,H],
        domain(X,0,200),
        1*A + 2*B + 5*C + 10*D + 20*E + 50*F + 100*G + 200*H #= 200,
        labeling([ffc,enum],X).


coins_d(Coins, Money,M, Sum) :-
        length(Coins,Len),
        (M =:= Len ->
             Sum = 1
        ;
          ( for(I,M,Len),
            fromto(0,In,Out,Sum),
            param(Money,Coins) do
              nth1(I,Coins,CoinsI),
              Money_CoinsI is Money - CoinsI,
              ( Money_CoinsI =:= 0 ->
                  Out is In + 1
              ;
                  ( Money_CoinsI > 0 ->
                       coins_d(Coins,Money_CoinsI,I,Sum2),
                       Out is In + Sum2
                  ;
                       Out = In
                  )
              )
          )
        ).

