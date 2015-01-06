% 
% Reversed cryptograms in MiniZinc.
% 
% (Requires MiniZinc 2.0)
%
% Find the minimum construction of the following
%  - length of x = n
%  - digits 0..9
%  - all_different(x)
%  - all_different(z)
%  - x + reversed(x) = z
%  - x[1] > 0 and x[n] > 0
%
% How many optimal solutions are there for different n?

%
% With symmetry breaking x[1] < x[n]:
%  n  #sols
% 
%  1     0
%  2     0
%  3    42
%  4   260
%  5   584
%  6   504
%  7  1648
%  8  2016
%  9   992
% 10  1152

% Note: 
% Not all FlatZinc solvers support decision variables of size pow(10,10)-1.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 9;

% decision variables
array[1..n] of var 0..9: x; 
array[1..n] of var 0..9: z; % then sum of x + reversed(x)

var pow(10,n-1)..pow(10,n)-1: vx = to_num(x);
var pow(10,n-1)..pow(10,n)-1: vz = to_num(z);


% returns the reversed array of a
function array[int] of var int: reverse(array[int] of var int: a) =
   [a[length(a)-i+1] | i in 1..length(a)]
;

% n = to_num_base(a, base)
function var int: to_num_base(array[int] of var int: a, int: base) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(base, len-i) * a[i] 
                 );
         } in n
;

% to_num base 10
function var int: to_num(array[int] of var int: a) = to_num_base(a, 10);

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
    all_different(x) /\
    all_different(z) /\
    vz = vx + to_num(reverse(x)) /\
    x[1] > 0 /\ x[n] > 0 /\
    z[1] > 0 /\ 

    % symmetry breaking
    x[1] < x[n]

;

output [
  "x : ", show(vx), "\n",
  "xr: ", join("", [show(fix(x[n-i+1])) | i in 1..n]), "\n",
  "vz: ", show(vz), "\n",

];

