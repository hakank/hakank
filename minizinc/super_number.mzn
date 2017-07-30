% 
% The Super number in MiniZinc.
%
%  
% From https://www.logisch-gedacht.de/logikraetsel/10stellige-zahl/ (German) 
% Translated:
% """
% The Super number
%
% We are looking for the 10-digit number which satisfies the following conditions:
%
% All digits from 0-9 occur exactly once.
% The first 2 digits are divisible by 2.
% The first 3 digits are divisible by 3.
% ...
% The first 10 digits are divisible by 10. 
% """"

%
% Solution: 3816547290
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

% decision variables
array[1..n] of var 0..9: x;

% n = to_num_base(a, base)
function var int: to_num_base(array[int] of var int: a, int: base) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(base, len-i) * a[i] 
                 );
         } in n
;

% base 10
function var int: to_num(array[int] of var int: a) = to_num_base(a, 10);


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x) /\
  forall(i in 2..n) (
     to_num([x[j] | j in 1..i]) mod i = 0
  )
;

output 
[
  join("",[show(fix(x[i])) | i in 1..n])
];

