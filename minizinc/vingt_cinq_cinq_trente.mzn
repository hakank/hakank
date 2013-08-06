% 
% Alphametic puzzle in MiniZinc.
% 
% Martin Gardner (November 1975)
% """
%  VINGT
%   CINQ
% + CINQ
% -----
% TRENTE
%"""

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";
var 0..9: V;
var 0..9: I;
var 0..9: N;
var 0..9: G;
var 0..9: T;
var 0..9: C;
var 0..9: Q;
var 0..9: R;
var 0..9: E;

array[1..9] of var int : fd = [V,I,N,G,T,C,Q,R,E];


constraint
  all_different(fd) /\
            10000*V + 1000*I + 100*N + 10*G + T  +  
                      1000*C + 100*I + 10*N + Q  +
                      1000*C + 100*I + 10*N + Q  = 
 100000*T + 10000*R + 1000*E + 100*N + 10*T + E
  /\
  V >= 1 /\
  C >= 1 /\
  T >= 1
;  



solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

output
[
  show(fd)
];