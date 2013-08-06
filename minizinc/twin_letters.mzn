% 
% Twin letters problem in MiniZinc.
% 
% From
% http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
% """
% Twin Letters    
%
% In the following puzzle, there are ten pairs of
% letters to be assigned to the same digit so that the multiplication
% (including intermediate results) is correct. Can you find out the
% pairs and their values?
%
%         A B C
%  *      D E F
%  ____________
%         G H I
%       J K L
%     M N O
%  ____________
%     P Q R S T
% """
%
% Compare with other models:
% * ECLiPSe : http://www.hakank.org/eclipse/twin_letters.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/twin_letters.pl
% * Gecode: http://www.hakank.org/gecode/twin_letters.cpp

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 20;

var 0..9: A;
var 0..9: B;
var 0..9: C;
var 0..9: D;
var 0..9: E;
var 0..9: F;
var 0..9: G;
var 0..9: H;
var 0..9: I;
var 0..9: J;
var 0..9: K;
var 0..9: L;
var 0..9: M;
var 0..9: N;
var 0..9: O;
var 0..9: P;
var 0..9: Q;
var 0..9: R;
var 0..9: S;
var 0..9: T;

array[1..n] of var 0..9: x = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T];

var 0..1: C1;
var 0..2: C2;
var 0..1: C3;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
    % exact 2 occurrences of each digit
    forall(i in 0..9) (
      count(x, i, 2)
    )
;

constraint
             100*G + 10*H + I +
    1000*J + 100*K + 10*L +
    10000*M + 1000*N + 100*O =
    10000*P + 1000*Q + 100*R + 10*S + T      /\
    
    (100*D + 10*E + F)*C = 100*G + 10*H + I  /\ 
    (100*D + 10*E + F)*B = 100*J + 10*K + L  /\
    (100*D + 10*E + F)*A = 100*M + 10*N + O  /\
    
    (100*A + 10*B + C) * (100*D + 10*E + F) =
    10000*P + 1000*Q + 100*R + 10*S + T
;

constraint
    % carry restrictions
    T         = I               /\
    S + 10*C1 = H + L           /\
    R + 10*C2 = G + K + O + C1  /\
    Q + 10*C3 = J + N + C2      /\
    P         = M + C3
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "C1: " ++ show(C1) ++ "\n" ++
  "C2: " ++ show(C2) ++ "\n" ++
  "C3: " ++ show(C3) ++ "\n"
];