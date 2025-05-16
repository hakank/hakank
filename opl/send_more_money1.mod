/* 

  SEND+MORE=MONEY problem in OPL.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 8;
range digits = 0..9;

dvar int S in digits;
dvar int E in digits;
dvar int N in digits;
dvar int D in digits;
dvar int M in digits;
dvar int O in digits;
dvar int R in digits;
dvar int Y in digits;

dvar int x[1..n] = [S,E,N,D,M,O,R,Y];

constraints {

  allDifferent(x);

           (1000*S +100*E +10*N + D) +
           (1000*M +100*O +10*R + E) ==
  (10000*M +1000*O +100*N +10*E + Y);
  
  S > 0;
  M > 0;

}

execute {
   writeln(x);
} 
