%
% DONALD+GERALD=ROBERT problem in Minizinc.
% 
% Using carries.
%
%
% Send More Money
% c4 c3 c2 c1  
%    S  E  N  D
%  + M  O  R  E
%  ------------
% M  O  N  E  Y
%

% c6 c5 c4 c3 c2 c1
%    D  O  N  A  L  D
% +  G  E  R  A  L  D
% -------------------
%    R  O  B  E  R  T

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

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

array[1..10] of var int : fd =
   [D,O,N,A,L,G,E,R,B,T];

var 0..1: c1;
var 0..1: c2;
var 0..1: c3;
var 0..1: c4;
var 0..1: c5;
var 0..1: c6;


constraint
  all_different(fd) /\

       D + D = 10 * c1 + T /\
  c1 + L + L = 10 * c2 + R /\
  c2 + A + A = 10 * c3 + E /\
  c3 + N + R = 10 * c4 + B /\
  c4 + O + E = 10 * c5 + O /\
  c5 + D + G = 10 * c6 + R /\
  
  D > 0 /\
  G > 0 /\
  R > 0 /\
  c6 = 0 % must be 0 since R is the first digit
;  

solve satisfy;
% solve :: int_search(fd, smallest, indomain_min, complete) satisfy;

output [
   " D  O  N  A  L  G  E  R  B  T\n",
   show(fd), "\n",
   "\n\n",

   "D:", show(D), " O:", show(O), " N:", show(N), " A:", show(A), " L:",show(L), " D:", show(D),"\n",
   "G:", show(G), " E:", show(E), " R:", show(R), " A:", show(A), " L:",show(L), " D:", show(D),"\n",
   "R:", show(R), " O:", show(O), " B:", show(B), " E:", show(E), " R:",show(R), " T:", show(T),"\n",
   "\n\n",

   "   ",  show(c6),show(c5),show(c4),show(c3),show(c2),show(c1),"\n",
   "    ", show(D), show(O), show(N), show(A), show(L),show(D),"\n",
   "  + ", show(G), show(E), show(R), show(A), show(L),show(D),"\n",
   "  = ", show(R), show(O), show(B), show(E),show(R), show(T),"\n",
   "\n\n",
   "c6:", show(c6)," c5:", show(c5)," c4:", show(c4)," c3:", show(c3)," c2:", show(c2)," c1:", show(c1),


];
