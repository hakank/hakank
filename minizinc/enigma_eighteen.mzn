% 
% Enigma Eighteen puzzle (#1496) in MiniZinc.
%
% From http://www.f1compiler.com/samples/Enigma%201496.f1.html
% """
%
%  Eighteen
%
%  Enigma 1496 Albert Haddad, New Scientist magazine, May 31, 2008
%
%
%  An eight digit "multiplicand" comprising of dots and letters is multiplied
%  by 2 resulting in the product EIGHTEEN. Different letters stand for 
%  different digits, the same letter stands for the same digit, and a dot can 
%  be any digit. 
%
%      . . N I N E . .
%                  * 2
%     ----------------
%   =  E I G H T E E N
%     ----------------
%
%  What is the eight-digit product EIGHTEEN?
%
% """
%         EIGHTEEN
% Answer: 91637998

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 0..9;
var r: N;
var r: I;
var r: E;
var r: G;
var r: H;
var r: T;
var r: X1;
var r: X2;
var r: X3;
var r: X4;
array[1..10] of var r: x = [N,I,E,G,H,T,X1,X2,X3,X4];
array[1..8] of var r: nine = [X1,X2,N,I,N,E,X3,X4];
array[1..8] of var r: eighteen = [E,I,G,H,T,E,E,N];

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different([N,I,E,G,H,T]) 
  /\
  E >= 1
  /\
  X1 >= 1
  /\
  2*(10000000*X1 + 1000000*X2 + 100000*N + 10000*I + 1000*N + 100*E + 10*X3 + X4)  = 
   ( 10000000*E  + 1000000*I  + 100000*G + 10000*H + 1000*T + 100*E + 10*E  + N)


;

output [
  "x: ", show(x), "\n",
  "..NINE..: ", show(nine), "\n",
  "EIGHTEEN: ", show(eighteen),"\n",
  "(", show(X1),show(X2),show(N),show(I),show(N),show(E),show(X3),show(X4), ") * 2 = ", 
  show(E),show(I),show(G),show(H),show(T),show(E), show(E), show(N),"\n"

];