% 
% Fantasy OR in MiniZinc.
% 
% Problem and XPress Mosel model from
% Martin J Chlond "Fantasy OR"
% http://archive.ite.journal.informs.org/Vol4No3/Chlond/index.php
% 
% Note in the original XPress Mosel model 
% Filename    : sangraal.mos
% Written by  : Martin J Chlond
% Date written: 10-April-2002
% Source      : http://www.amsta.leeds.ac.uk/~pmt6jrp/personal/tms.html


% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: k = 8;

set of int: knight = 1..k;
set of int: posit = 1..k;

array[knight] of int: free; % time to free each knight
array[knight] of int: prep; % time to prepare each knight

array[knight, posit] of var 0..1: x; % x(i,j)=1 if knight i in position j, 0 otherwise
array[posit] of var 0..1: d; % d(j)=1 if position j finished within 20 minutes, 0 otherwise
array[posit] of var int: t; % finish time for each position

% maximise number of positions finished within 20 minutes
var int: maxk = sum(j in posit) (d[j]);

solve :: int_search([x[i,j] | i in knight, j in posit ], 
        first_fail, indomain_min, complete) maximize maxk;

constraint  
  % each knight in one position
  forall(i in knight) (
     sum(j in posit) (x[i,j]) = 1     
  )
  /\
  % each position has one knight
  forall(j in posit) (
     sum(i in knight) (x[i,j]) = 1 
  )
  /\
  % compute finish time for each position
  forall(j in posit) (
    (sum(i in knight, l in 1..j-1) (free[i]*x[i,l]) + 
     sum(i in knight) ((free[i]+prep[i])*x[i,j])) = t[j]
  )
  /\
  % d(j) = 1 if knight in position j is freed and prepared within 20 minutes
  forall(j in posit) (
    t[j] >= 21-15*d[j]
    /\
    t[j] <= 53-33*d[j]
  )
;

output 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in knight, j in posit
]
++
[
  "\nt: ", show(t) ++ "\n",
  "d: ", show(d) ++ "\n",
  "maxk: ", show(maxk)
];

%
% data
%
free = [1, 1, 2,2, 3, 4, 5,6];
prep = [15,5,15,5,10,15,10,5];



 