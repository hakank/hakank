% 
% Planning model in MiniZinc.
% 
% From GLPK:s example plan.mod
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


var 0.0..200.0: bin1;
var 0.0..2500.0: bin2;
var 400.0..800.0: bin3;
var 100.0..700.0: bin4;
var 0.0..1500.0: bin5;
var float: alum; % , >= 0;
var float: silicon; % , >= 0;

var float: value = 0.03 * bin1 + 0.08 * bin2 + 0.17 * bin3 + 0.12 * bin4 + 0.15 * bin5 + 0.21 * alum + 0.38 * silicon;

% solve minimize value;
solve :: float_search(
         [bin1,bin2,bin3,bin4,bin5,alum,silicon], 0.0001, 
         input_order, indomain_split,complete) 
     minimize value;

constraint

  alum >= 0.0
  /\
  silicon >= 0.0
  /\
  bin1 + bin2 + bin3 + bin4 + bin5 + alum + silicon = 2000.0
  /\
  0.15 * bin1 + 0.04 * bin2 + 0.02 * bin3 + 0.04 * bin4 + 0.02 * bin5 +
      0.01 * alum + 0.03 * silicon <= 60.0
  /\
  0.03 * bin1 + 0.05 * bin2 + 0.08 * bin3 + 0.02 * bin4 + 0.06 * bin5 +
      0.01 * alum <= 100.0
  /\ 
  0.02 * bin1 + 0.04 * bin2 + 0.01 * bin3 + 0.02 * bin4 + 0.02 * bin5 <= 40.0
  /\ 
  0.02 * bin1 + 0.03 * bin2 + 0.01 * bin5 <= 30.0
  /\ 
  0.70 * bin1 + 0.75 * bin2 + 0.80 * bin3 + 0.75 * bin4 + 0.80 * bin5 + 0.97 * alum >= 1500.0
  /\ 
  250.0 <= 0.02 * bin1 + 0.06 * bin2 + 0.08 * bin3 + 0.12 * bin4 + 0.02 * bin5 +     0.01 * alum + 0.97 * silicon 
  /\
  0.02 * bin1 + 0.06 * bin2 + 0.08 * bin3 + 0.12 * bin4 + 0.02 * bin5 + 
     0.01 * alum + 0.97 * silicon <= 300.0

;

output [
"bin1: " ++ show(bin1) ++ "\n"++
"bin2: " ++ show(bin2) ++ "\n"++
"bin3: " ++ show(bin3) ++ "\n"++
"bin4: " ++ show(bin4) ++ "\n"++
"bin5: " ++ show(bin5) ++ "\n"++
"alum: " ++ show(alum) ++ "\n"++
"silicon: " ++ show(silicon) ++ "\n"++
"value: " ++ show(value) ++ "\n"
];
