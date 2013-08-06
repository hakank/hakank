/*

  Curious numbers puzzle in SICStus Prolog.

 """
  Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.

  The number 48 has this peculiarity, that if you add 1 to it the result
  is a square number, and if you add 1 to its half, you also get a
  square number. Now, there is no limit to the numbers that have this
  peculiarity, and it is an interesting puzzle to find three more of
  them---the smallest possible numbers. What are they?
  """ 


  The least such numbers are: 
  [
   [48,49,7,24,25,5],
   [1680,1681,41,840,841,29],
   [57120,57121,239,28560,28561,169], 
   [1940448,1940449,1393,970224,970225,985]
  ]


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/cur_num.mzn
  * Comet   : http://www.hakank.org/comet/cur_num.co
  * ECLiPSe : http://www.hakank.org/eclipse/cur_num.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go:-
     findall(X,curious(X),Z), 
     ( foreach(C,Z) do
           write(C),nl
     ),
     fd_statistics.


curious(LD) :-
  LD = [X,A,B,C,D,E],
  domain(LD,1,2000000),
  X + 1 #= A, % if you add 1 to it 
  A #= B * B, % the result is a square number
  
  X #= 2 * C, % if you to its half
  C + 1 #= D, % add 1 
  D #= E * E, % you also get a square number   
  labeling([],LD).
