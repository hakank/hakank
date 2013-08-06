/*

  Coin minimization in SICStus Prolog.

  Which coins to use if we have to pay one exact sum and
  as few coins as possible.

  Here we use the swedish coins before the big coin reforms (there has
  been a couple of them).

      1 kr     OneKr          (100 oere)
     50 oere   FiftyOre
     25 oere   TwentyFiveOre
     10 oere   TenOre
      5 oere   FiveOre
      1 oere   OneOre


  Compare with the following model:
  * ECLiPSe: http://www.hakank.org/eclipse/changes.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% changes is in oere (100 oere in 1 kr)
% 
go:-
        Change = 123,
        findall(
                [Change,NumCoins,LD],
                changes(LD,NumCoins,Change),L),
        write(L),nl.


changes([OneKr,FiftyOre,TwentyFiveOre,TenOre,FiveOre,OneOre],NumCoins,Change) :-

        LD = [OneKr, FiftyOre, TwentyFiveOre,TenOre,FiveOre, OneOre],

        domain(LD, 0,100),

        % the value of coins, in oere
        100*OneKr+50*FiftyOre+25*TwentyFiveOre+10*TenOre+5*FiveOre+1*OneOre #= Change,

        % number of coins, the weights which we will minimize
        OneKr+FiftyOre+TwentyFiveOre+TenOre+FiveOre+OneOre #= NumCoins,

        % labeling([], LD).
        labeling([minimize(NumCoins)], LD).



% check changes for some prime numbers
go2:-
        List = [2,3,5,7,11,13,17,23,29,31],
        ( foreach(P,List) do
              changes(LL,NumCoins,P),
              format('Change: ~d, Number of coins: ~d, Coins: ~w', [P, NumCoins,LL]),nl
        ).
