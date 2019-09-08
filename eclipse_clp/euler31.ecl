%
% Problem 31
% """
% In England the currency is made up of pound, £, and pence, p, and 
% there are eight coins in general circulation:
%
%    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
%
% It is possible to make £2 in the following way:
%
%    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
%
% How many different ways can £2 be made using any number of coins?
%
% """
% 
% Answer: 73682
% 
%  - plain Prolog (problem31/0): 0.12s
%  - CLP (problem31b): with best labeling (first_fail/indomain) 0.39s
% 
% This algorithm is based on my SETL version
% http://www.hakank.org/setl/project_euler31.setl
%

:- lib(listut).
:- lib(ic).
:- lib(util). % time/1

go :-
  time(problem31),
  time(problem31b),
  time(problem31c).

p31_coins(Coins, Money,M, Sum) :-
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
                       p31_coins(Coins,Money_CoinsI,I,Sum2),
                       Out is In + Sum2
                  ;
                       Out = In
                  )
              )
          )
        ).

% (0.14s)
problem31 :-
        Coins = [200,100,50,20,10,5,2,1],
        p31_coins(Coins,200,1,T),
        writeln(T).

%
% My variant of the CLP version from
% http://thisisnotaprogrammersblog.blogspot.com/2008/11/project-euler.html 
% (see problem31c/0 below)
% first_fail/indomain_min: 0.63s  first_fail/indomain: 0.38s
%
% (0.38s)
problem31b :-
        p31_ways(200,N),
        writeln(N).

p31_currency(List, Sum) :-
     List = [N1,N2,N5,N10,N20,N50,N100,N200],
     List :: 0..200,
     Sum #= N1 + N2 * 2 + N5 * 5 + N10 * 10 + N20 * 20 + N50 * 50 +
            N100 * 100 + N200 * 200,
     search(List, 0, first_fail, indomain, complete, []).

p31_ways(Sum, Number) :-
     findall(List, p31_currency(List, Sum), Ways),
     length(Ways, Number).


%
% Problem 31 (using CLP)
% From http://thisisnotaprogrammersblog.blogspot.com/2008/11/project-euler.html
% 
% Solution: 73682 (2.29s, slower)
% (2.29s)
problem31c :-
        p31_waysc(200,N),
        writeln(N).

p31_currencyc(List, Sum) :-
     List = [N1,N2,N5,N10,N20,N50,N100,N200],
     Sum #= N1 + N2 * 2 + N5 * 5 + N10 * 10 + N20 * 20 + N50 * 50 +
            N100 * 100 + N200 * 200,
     N1 #>= 0, N2 #>= 0, N5 #>= 0, N10 #>= 0,
     N20 #>= 0, N50 #>= 0, N100 #>= 0, N200 #>= 0,
     labeling(List).

p31_waysc(Sum, Number) :-
     findall(List, p31_currencyc(List, Sum), Ways),
     length(Ways, Number).
