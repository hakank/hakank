/*

  Subset sum problem in B-Prolog.

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
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        
        N = 6,
        Total = 100,
        Coins = [16, 17, 23, 24, 39, 40],
        length(Coins,Len),

        length(X,Len), 
        X :: 0..N,

        subset_sum(Coins, X, Total),
        NumStolen #= sum(X), % total number of bags stolen

        labeling(X),

        writeln(coins:Coins),
        writeln(total:Total),
        writeln(x:X),
        writeln(num_stolen:NumStolen).


%
% subset_sum(Values, X, Tot) 
%  where 
%    values is the values to choose from (the coin values)
%    x contatins the resulting var 
%    total is the total value to sum
%
subset_sum(Values, X, Tot) :-
        scalar_product(Values,X,#=,Tot).

