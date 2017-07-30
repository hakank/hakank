% 
% Scheduling example (Bratko) in MiniZinc.
% 
% Scheduling example from 
% Ivan Bratko "Prolog - Programming for Artificial Intelligence", 3rd edition,
% page 320f.
% This is the first example in the chapter 'Constraint satisfaction and logic programming'.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var 0..10: Ta; % using ints
var 0..10: Tb;
var 0..10: Tc;
var 0..10: Td;
var 0..10: Tf;


solve minimize Tf;
% solve satisfy;

constraint

  Ta + 2 <= Tb
  /\
  Ta + 2 <= Tc
  /\ 
  Tb + 3 <= Td
  /\
  Tc + 5 <= Tf
  /\
  Td + 4 <= Tf

  % /\ % for solve satisfy, shows all three solutions with an apropriate solver
  % Tf <= 9 
;

