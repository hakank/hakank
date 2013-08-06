/*

  Added corner puzzle in ECLiPSe.

 
  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  """
  This puzzle requires that you enter the digits 1 through 8 in the circles and 
  squares (one digit in each figure) so that the number in each square is equal 
  to the sum on the numbers in the circles which  adjoin it.  
  ...
  
     C F C
     F   F
     C F C
  """


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).


go :-
        Digits = 1..8,

        X = [A,B,C,D,E,F,G,H],
        X :: Digits,

        alldifferent(X),
        B #= A + C,
        D #= A + F,
        E #= C + H,
        G #= F + H,


        labeling(X),

        printf("%d %d %d\n", [A,B,C]),
        printf("%d   %d\n", [D,E]),
        printf("%d %d %d\n", [F,G,H]),
        nl,
        fail.

        

