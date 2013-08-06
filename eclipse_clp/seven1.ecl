/*

  7.11 puzzle in ECLiPSe.

  From
  http://www-lp.doc.ic.ac.uk/UserPages/staff/ft/alp/humour/num/seven-1.html

  """
  Appeared in Volume 9/1, February 1996
  A man goes into a store and selects four items to purchase. He walks up to the
  counter to pay and the clerk says 
  "Hold on, my cash register is broken, so I have to use a calculator to get 
  your total... okay, that'll be $7.11" 
  The man pays, and as he is walking out, the clerk yells "Wait a second! I
  multiplied the prices together instead of adding them. Let me get
  the total again... hey, what do you know! It comes out the same!"

  What were the prices of the four items? All of them are of the form a.bc, that 
  is, we're dealing with standard U.S. money, no fractions of a cent. Ignore
  sales taxes too.

  Some discussion of this appeared in the sci.math newsgroup under the topic
  "8th grade math puzzler", since the puzzle was originally given to a class of
  8th Graders.

  John Shonder
  padrote@delphi.com
  """
  

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:- lib(ic), lib(ic_search).

go :-   findall([A+B+C+D=S,A*B*C*D/10000],seven_eleven([A,B,C,D,S]),LL),
        writeln(LL).

seven_eleven([A,B,C,D,S]) :-
     S = 711,
     L = [A,B,C,D],
     L :: [0..711],
     
     A * B * C * D #= S*100*100*100,
     A + B + C + D #= S,
     
     % symmetry removal
     A #< B,
     B #< C,
     C #< D, 

     search(L,0,largest,indomain_split,complete,[]).
     % search(L,0,largest,indomain_split,bbs(7),[]).

