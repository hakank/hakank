/*

  Coin minimization in SWI Prolog

  Which coins to use if we have to pay one exact sum and
  as few coins as possible.

  Here we use the Swedish coins before the big coin reforms (there has
  been a couple of them).

      1 kr     OneKr          (100 oere)
     50 oere   FiftyOre
     25 oere   TwentyFiveOre
     10 oere   TenOre
      5 oere   FiveOre
      1 oere   OneOre


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% changes is in oere (100 oere in 1 kr)
% 
go:-
        Change = 123,
        once(changes(LD,NumCoins,Change)),
        writeln([ld=LD,num_coins=NumCoins,change=Change]),
        nl.

% minimal changes for 1..100
go2:-
        between(1,100,Change),
        once(changes(LL,NumCoins,Change)),
        write([change:Change, numcoins:NumCoins,coins:LL]),nl,
        fail.

go2.

changes([OneKr,FiftyOre,TwentyFiveOre,TenOre,FiveOre,OneOre],NumCoins,Change) :-

        LD = [OneKr, FiftyOre, TwentyFiveOre,TenOre,FiveOre, OneOre],

        LD ins 0..100,

        % the value of coins, in oere
        100*OneKr+50*FiftyOre+25*TwentyFiveOre+10*TenOre+5*FiveOre+1*OneOre #= Change,

        % number of coins, the weights which we will minimize
        OneKr+FiftyOre+TwentyFiveOre+TenOre+FiveOre+OneOre #= NumCoins,

        labeling([min(NumCoins)], LD).


