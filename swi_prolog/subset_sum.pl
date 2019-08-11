/*

  Subset sum problem in SWI Prolog

  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :- 

    N = 6,
    Total = 100,
    Coins = [16, 17, 23, 24, 39, 40],
    length(Coins,Len),

    %% How many of each coin where stolen?
    length(X,Len), 
    X ins 0..N,

    scalar_product(Coins, X, #=, Total),
    sum(X,#=,NumStolen), %% total number of bags stolen

    label(X),

    writeln(coins=Coins),
    writeln(total=Total),
    writeln(x=X),
    writeln(num_stolen=NumStolen),
    findall(C,
            (between(1,N,I),element(I,X,XI), XI #> 0,element(I,Coins,C)),
            Stolen),
    format("Stolen coin bags: ~w~n", [Stolen]),
    nl.

