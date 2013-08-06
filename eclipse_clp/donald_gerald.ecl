/*

  DONALD + ROBERT = GERALD in ECLiPSe.

  Clasic alphametic problem


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

go :-

        X = [D,O,N,A,L,G,E,R,B,T],
        X :: 0..9,

        alldifferent(X),

           100000*D + 10000*O + 1000*N + 100*A + 10*L + D 
         + 100000*G + 10000*E + 1000*R + 100*A + 10*L + D
        #= 100000*R + 10000*O + 1000*B + 100*E + 10*R + T,

        D #> 0,
        G #> 0,
        R #> 0,

        labeling(X),

        writeln(X).


