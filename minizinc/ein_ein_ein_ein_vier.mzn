% 
% Alphametic puzzle in MiniZinc.
% 
% Martin Gardner (November 1975)
% """
%  EIN
%  EIN
%  EIN
%  EIN
% -----
% VIER
%"""

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";
var 0..9: E;
var 0..9: I;
var 0..9: N;
var 0..9: V;
var 0..9: R;

array[1..5] of var int : fd = [E,I,N,V,R];


constraint
  all_different(fd) /\
                      100*E + 10*I + N +
                      100*E + 10*I + N +
                      100*E + 10*I + N +
                      100*E + 10*I + N =
             1000*V + 100*I + 10*E + R
  /\
  E >= 1 /\
  V >= 1
;  



solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

output
[
  "fd: " ++ show(fd)
];