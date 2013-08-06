/*

  Subset sum problem in SICStus Prolog.

  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """

  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/subset_sum.mzn
  * Comet   : http://www.hakank.org/comet/subset_sum.co
  * ECLiPSe : http://www.hakank.org/eclipse/subset_sum.ecl
  * Gecode  : http://www.hakank.org/gecode/subset_sum.cpp
  * Tailor/Essence': http://www.hakank.org/tailor/subset_sum.eprime


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-lib(ic).

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        
        N = 6,
        Total = 100,
        Coins = [16, 17, 23, 24, 39, 40],
        length(Coins,Len),

        length(X,Len), 
        domain(X, 0, N),

        subset_sum(Coins, X, Total),
        sum(X, #=, NumStolen), % total number of bags stolen

        labeling([],X),

        write(coins:Coins),nl,
        write(total:Total),nl,
        write(x:X),nl,
        write(num_stolen:NumStolen),nl,nl,
        fd_statistics.


%
% subset_sum(Values, X, Tot) 
%  where 
%    values is the values to choose from (the coin values)
%    x contatins the resulting var 
%    total is the total value to sum
%
subset_sum(Values, X, Tot) :-
        scalar_product(Values,X,#=,Tot).


