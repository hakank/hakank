% 
% Enigma problem 1555 (Not a square) in MiniZinc.
% 
% From New Scientist:
% """
% Enigma Number 1555
%
% July 25, 2009 by Richard England
%
% Not a square
%
% Fill each of the 12 cells 
%
%      +--+--+
%      |  |  |  
%   +--+--+--+--+
%   |  |  |  |  |  
%   +--+--+--+--+
%   |  |  |  |  |  
%   +--+--+--+--+
%      |  |  |  
%      +--+--+
% 
% with a digit so that the two four-digit numbers that you can read down, the 
% two two-digit numbers that you can read down at the sides and the two-digit 
% number that you can read across at the bottom are all different perfect 
% squares (with no leading zeros). Only the two-digit number that you can read
% across at the top is not a perfect square.
% What is that two-digit number?
% """
%
% The representation of the grid is
%     A B
%   C D E F
%   G H I J
%     K L
% 
% Compare with this F1 Compiler model:
% http://www.f1compiler.com/samples/Enigma%201555.f1.html
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:% www.hakank.org/minizinc
%

% include "globals.mzn"; 
set of int: Digit = 0..9;

var Digit: A;
var Digit: B;
var Digit: C;
var Digit: D;
var Digit: E;
var Digit: F;
var Digit: G;
var Digit: H;
var Digit: I;
var Digit: J;
var Digit: K;
var Digit: L;

var 10..99: AB = A*10 + B;

array[1..12] of var 0..9999: all = [A,B,C,D,E,F,G,H,I,J,K,L];

predicate square(var int: x) =
   let {
      var 0..ceil(sqrt(int2float(ub(x)))): y
   } in
   y*y = x
;

solve satisfy;

constraint
    % leading digits
    A > 0 /\ C > 0 /\ G > 0 /\ K > 0 /\ B > 0 /\ F > 0 

    /\
    square(C*1000+D*100+E*10+F) /\ % CDEF
    square(G*1000+H*100+I*10+J) /\ % GHIJ
    square(C*10+G)              /\ % CG
    square(F*10+J)              /\ % FJ
    square(K*10+L)              /\ % KL
    square(A*1000+D*100+H*10+K) /\ % ADHI
    square(B*1000+E*100+I*10+L)    % BEIL

    /\ % and AB is not a square
    not exists(tmp in 1..10) (
      tmp*tmp = AB
    )
;

output [
  "A: " ++ show(A) ++ "\n" ++
  "B: " ++ show(B) ++ "\n" ++
  "C: " ++ show(C) ++ "\n" ++
  "D: " ++ show(D) ++ "\n" ++
  "E: " ++ show(E) ++ "\n" ++
  "F: " ++ show(F) ++ "\n" ++
  "G: " ++ show(G) ++ "\n" ++
  "H: " ++ show(H) ++ "\n" ++
  "I: " ++ show(I) ++ "\n" ++
  "J: " ++ show(J) ++ "\n" ++
  "K: " ++ show(K) ++ "\n" ++
  "L: " ++ show(L) ++ "\n" ++

   "AB: "   ++  show(AB)   ++ "\n" ++

  "all: [A  B  C  D  E  F  G  H  I  J  K  L]\n" ++
  show([A,B,C,D,E,F,G,H,I,J,K,L]) ++ "\n"
] ++ ["\n"];
