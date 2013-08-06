/*

  Curious numbers in ECLiPSe:

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



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:-lib(ic).


go:-
     findall(X,curious(X),Z), 
     write(Z),nl.


curious(LD) :-
  LD = [X,A,B,C,D,E],
  LD :: 1..2000000,
  X + 1 #= A, % if you add 1 to it 
  A #= B * B, % the result is a square number
  
  X #= 2 * C, % if you to its half
  C + 1 #= D, % add 1 
  D #= E * E, % you also get a square number   
  search(LD,0,first_fail,indomain,complete,[]).
