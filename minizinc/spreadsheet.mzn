% 
% Spreadsheet in MiniZinc.
% 
% From Krzysztof Apt "Principles of Constraint Programming" page 16ff. Spreadsheet.
% Cf Winston "Artificial Intelligence", 3rd edition, page 235 
% (not the same values though)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% ECLiPSe/ic solver gives the following result:
% B1 = 0.17__0.17
% B4 = 3.5__3.5
% B5 = 1.7__1.7
% C4 = 1.5__1.5
% C5 = 4.5__4.5
% D4 = 5.25__5.25
% D5 = 7.6499999999068677__7.65
% E7 = 12.899999999906868__12.9
% E8 = 15.092999999891033__15.093000000000004
% Total time 0.010s cpu (0.010 setup + 0.000 search)


var 0.0..1000.0: B1;
var 0.0..1000.0: B4;
var 0.0..1000.0: B5;
var 0.0..1000.0: C4;
var 0.0..1000.0: C5;
var 0.0..1000.0: D4;
var 0.0..1000.0: D5;
var 0.0..1000.0: E7;
var 0.0..1000.0: E8;

solve satisfy;

constraint
      B1 = 0.17 /\
      B4 = 3.5 /\
      B5 = 1.7 /\
      C4 = 1.5 /\
      C5 = 4.5 /\
      D4 = B4 * C4 /\
      D5 = B5 * C5 /\
      E7 = D4 + D5 /\
      E8 = E7 * (1.0 + B1) 
;


output [
  show([B1, B4, B5,C4,C5,D4,D5,E7,E8]) 

];
