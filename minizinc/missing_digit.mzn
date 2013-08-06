% 
% Missing digit in MiniZinc.
%
% From Emissary (Mathematical Sciences Research Institute), Spring/Fall 2006
% http://www.msri.org/communications/emissary/pdfs/EmissarySpringFall2006.pdf -
% """
% The number 2^29 is a 9-digit number with distinct digits. 
% Which digit is missing?
% """
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

array[1..9] of var 0..9: x;
int: n = ceil(pow(2.0, 29.0));
var 0..9: missing;

predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


solve satisfy;

constraint
   toNum10(x, n)
   /\
   forall(i in 1..9) (
     missing != x[i]
   )
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "missing: " ++ show(missing) ++ "\n" 
];
