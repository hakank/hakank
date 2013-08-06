% 
% General store problem in MiniZinc.
% 
% 
% From
% http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
%
% """
% The General Store  from "Mathematical Puzzles of Sam Loyd", number 30
%
% The owner of a general store, who is something of a puzzlist, has put
% up this sign to see if any of his mathematical friends can translate
% it properly. Each different letter stands for a different digit. The
% words above the horizontal line represent numbers that add to the
% total of "ALL WOOL". The problem is to change all the letters to the
% correct digits.
%
%        C H E S S
%  +       C A S H
%  +   B O W W O W
%  +     C H O P S
%  +   A L S O P S
%  + P A L E A L E
%  +       C O O L
%  +       B A S S
%  +       H O P S
%  +       A L E S
%  +       H O E S
%  +   A P P L E S
%  +       C O W S 
%  +   C H E E S E
%  +   C H S O A P
%  +     S H E E P
%  _______________
%    A L L W O O L
%  """
%
%
% Compare with the following models:
% * SICStus Prolog: http://www.hakank.org/sicstus/general_store.pl
% * ECLiPSE: http://www.hakank.org/eclipse/general_store.ecl
% * Gecode:  http://www.hakank.org/gecode/general_store.cpp

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 10;

var 0..9: C;
var 0..9: H;
var 0..9: E;
var 0..9: S;
var 0..9: A;
var 0..9: B;
var 0..9: O;
var 0..9: W;
var 0..9: P;
var 0..9: L;

array[1..n] of var 0..n-1: x = [C, H, E, S, A, B, O, W, P, L];


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  all_different(x) /\

                            10000*C + 1000*H + 100*E + 10*S + S
   +                                  1000*C + 100*A + 10*S + H
   +             100000*B + 10000*O + 1000*W + 100*W + 10*O + W
   +                        10000*C + 1000*H + 100*O + 10*P + S
   +             100000*A + 10000*L + 1000*S + 100*O + 10*P + S
   + 1000000*P + 100000*A + 10000*L + 1000*E + 100*A + 10*L + E
   +                                  1000*C + 100*O + 10*O + L
   +                                  1000*B + 100*A + 10*S + S
   +                                  1000*H + 100*O + 10*P + S
   +                                  1000*A + 100*L + 10*E + S
   +                                  1000*H + 100*O + 10*E + S
   +             100000*A + 10000*P + 1000*P + 100*L + 10*E + S
   +                                  1000*C + 100*O + 10*W + S
   +             100000*C + 10000*H + 1000*E + 100*E + 10*S + E
   +             100000*C + 10000*H + 1000*S + 100*O + 10*A + P
   +                        10000*S + 1000*H + 100*E + 10*E + P   
   = 1000000*A + 100000*L + 10000*L + 1000*W + 100*O + 10*O + L
 

;

output
[
  show(x)
];
