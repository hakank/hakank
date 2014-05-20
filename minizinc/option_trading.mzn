% 
% Option trading in MiniZinc.
% 
% From the Prolog example in
% Marriott & Stuckey, Programming with Constraints, page 170ff

% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% var -10000.0..10000.0: S;
% var -10000.0..10000.0: C;
% var -10000.0..10000.0: E;
% var -10000.0..10000.0: P;

var float: S;
var float: C;
var float: E;
var float: P;


% M&S, page 173
predicate buy_call_payoff(var float: s, var float: c, var float: e, var float: p) =
   ( (0.0 <= s /\ 0.0 <= e / 100.0) <-> p = -c )

   /\
   (s >= e / 100.0) <-> p = 100.0 * s - e - c 
;

% solve satisfy;
% solve maximize P;
solve :: float_search([S,C,E,P], 0.00001, input_order, indomain_split, complete) maximize P;

constraint
  buy_call_payoff(S, C, E, P)
  /\ C = 200.0
  % /\ E = 300.0
  % /\ S = 3.0
;

output
[
  "S:", show(S), "\n",
  "C:", show(C), "\n",
  "E:", show(E), "\n",
  "P:", show(P), "\n",

];