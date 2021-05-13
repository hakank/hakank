% 
% Final student problem in MiniZinc.
% 
% 
% From Chris Smith's MathNewsletter #555
% """
% Mrs Krabappel gave her class a test
% and she’s almost marked them all, just
% one to go

% If the final student scores just 7% then
% the class average will be 90% but,
% even more remarkably, if they
% score 92% then they’ll be looking at a
% class average of 95% (and Mrs K will
% be in line for a bonus).
% How many students are in the class?
% """


% Run with
% $  minizinc final_student.mzn --solver optimathsat -D "n=17"
% n: 17 x: [100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 22.99999999999994, 0.0]

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; %  = 10;

array[1..n-1] of var 0.0..100.0: x;

% solve satisfy;
solve :: float_search(x, 0.0001, input_order, indomain_split, complete) satisfy;

constraint
decreasing(x) /\
(sum(x[1..n-1]) + 7) / n = 90 /\
(sum(x[1..n-1]) + 92) / n = 95

;

output [
  "n: \(n) x: \(x)\n"
];

