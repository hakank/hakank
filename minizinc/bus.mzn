% 
% Bus problem in MiniZinc.
% 
% rec.puzzles FAQ
% http://brainyplanet.com/index.php/Bus?PHPSESSID=051ae1e2b6df794a5a08fc7b5ecf8028
% """
% Putting 33 students in each bus, the teacher has one student left over. 
% Putting more in each bus, but always the same number in each bus, the 
% teacher has one bus left over. How many buses and students are there?
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: m = 10000;
var 1..m: a; % students
var 1..m: b; % number of buses
var 1..m: c; % number if students per bus for the second try
int: n = 33;



solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  (a-1) = b * n
  /\ 
  a = c * (b-1)
;

output
[
  show([a,b,c])
];