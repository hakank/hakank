%
% Another set_cvering problem in Minizinc
% 
% Problem from 
% Katta G. Murty: "Optimization Models for Decision Making", page 302f
% http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
% 
% 10 senators making a committee, where there must at least be one 
% representative from each group:
% group:        senators:
% southern      1 2 3 4 5
% northern      6 7 8 9 10
% liberals      2 3 8 9 10
% conservative  1 5 6 7
% democrats     3 4 5 6 7 9
% republicans   1 2 8 10
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "set_covering3_model.mzn";

%
% data;
%
num_groups = 6;
num_senators = 10;

% what group does a senator belong to
belongs =
array2d(1..num_groups, 1..num_senators, 
[
  1, 1, 1, 1, 1, 0, 0, 0, 0, 0,   % 1 southern
  0, 0, 0, 0, 0, 1, 1, 1, 1, 1,   % 2 northern
  0, 1, 1, 0, 0, 0, 0, 1, 1, 1,   % 3 liberals
  1, 0, 0, 0, 1, 1, 1, 0, 0, 0,   % 4 conservative
  0, 0, 1, 1, 1, 1, 1, 0, 1, 0,   % 5 democrats
  1, 1, 0, 0, 0, 0, 0, 1, 0, 1    % 6 republicans
]);


output [
  "Senator " ++ show(i) ++ ": " ++ show(x[i]) ++ "\n" 
  | i in 1..num_senators
]

