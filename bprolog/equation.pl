/*

  Solve the equation in B-Prolog.

  11x11=4 
  22x22=16 
  33x33=?
  
  This model solves the problem with four interpretations.
  
  (2013-03-11: I've seen this problem in my web server log the 
               last days. Don't know the origin.)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        interpretation1(X1),
        writeln(x1:X1),
        interpretation2(X2),
        writeln(x2:X2),
        interpretation3(X3),
        writeln(x3:X3),
        interpretation4(X4),
        writeln(x4:X4),
        nl.

interpretation1(X) :-
        X :: 0..10000,
        (1+1) * (1+1) #= 4,
        (2+2) * (2+2) #= 16,
        (3+3) * (3+3) #= X,
        labeling(X).

        
calc(X,Y,Z) :-
        length(X, Len),
        Xs @= [B : I in 0..Len-1, [B], B is integer(10**I)],
        scalar_product(X,Xs,#=,Y),
        sum(X) #= Z.

interpretation2(X) :-
        X :: 0..10000,
        N = 6,
        length(A, N), A :: 0..9,
        length(B, N), B :: 0..9,
        length(C, N), C :: 0..9,
        calc(A, 11*11, 4),
        calc(B, 22*22, 16),
        calc(C, 33*33, X),
        labeling(X).


s3(I, X) :-
        X #= 4**I.

interpretation3(X) :-
        X :: 0..10000,
        s3(1, 4),
        s3(2, 16),
        s3(3, X),
        labeling(X).


s4(I, X) :-
        X #= 4*(I**I).

interpretation4(X) :-
        X :: 0..10000,
        s4(1, 4),
        s4(2, 16),
        s4(3, X),
        labeling(X).

