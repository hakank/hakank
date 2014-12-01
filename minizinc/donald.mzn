%
% Donald puzzle in Minizinc
%
% Traditional puzzle: Solve the following alphametic equation
%
%  DONALD+GERALD=ROBERT
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";
var 0..9: D;
var 0..9: O;
var 0..9: N;
var 0..9: A;
var 0..9: L;
var 0..9: G;
var 0..9: E;
var 0..9: R;
var 0..9: B;
var 0..9: T;

array[1..10] of var int : FD =
   [D,O,N,A,L,G,E,R,B,T]
;  

constraint
     100000*D+10000*O+1000*N+100*A+10*L+D
   + 100000*G+10000*E+1000*R+100*A+10*L+D
   = 100000*R+10000*O+1000*B+100*E+10*R+T /\
   D > 0 /\
   G > 0 /\
   R > 0
;  

constraint all_different(FD);

solve satisfy;
% solve :: int_search(FD, first_fail, indomain_min, complete) satisfy;



output [
   " D  O  N  A  L  G  E  R  B  T\n",
   show(FD), "\n",
]


