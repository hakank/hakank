% 
% WRONG + WRONG = RIGHT (alphametic problem) in MiniZinc.
% 
% Alphametic problem:
%   WRONG + WRONG = RIGHT
% 
% 2 solutions is O is set to 0 (zero). If no such constraint the problem has
% 21 solutions.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

set of int: digits = 0..9;

var digits: W;
var digits: R;
var digits: O;
var digits: N;
var digits: G;
var digits: I;
var digits: H;
var digits: T;

array[1..8] of var int: LD = [W,R,O,N,G,I,H,T];

solve satisfy;

constraint
        all_different(LD) /\
        10000 * W + 1000 * R + 100 * O + 10 * N + G 
      + 10000 * W + 1000 * R + 100 * O + 10 * N + G
      = 10000 * R + 1000 * I + 100 * G + 10 * H + T

      /\ O = 0

;


output [
   show(W), show(R), show(O), show(N), show(G), " * ", 
   show(W), show(R), show(O), show(N), show(G), " = ", 
   show(R), show(I), show(G), show(H), show(T), "   ", "\n"

]
