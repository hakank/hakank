/*

  Curious numbers in SWI Prolog


  Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.
  """
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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        findall(X,curious(X),L),
        maplist(writeln,L),
        nl.

curious(LD) :-
        LD = [X,A,B,C,D,E],
        LD ins 1..2000000,
        X + 1 #= A,             % if you add 1 to it 
        A #= B * B,             % the result is a square number
        
        X #= 2 * C,             % if you to its half
        C + 1 #= D,             % add 1 
        D #= E * E,             % you also get a square number
        
        labeling([ffc,bisect], LD).

