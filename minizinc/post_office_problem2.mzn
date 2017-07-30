%
% Post office problem in Minizinc
%
% From http://www-128.ibm.com/developerworks/linux/library/l-glpk2/
%
% See post_office_problem.mzn. 
%
% Compared with post_office_problem.mzn this is a more general model
% which adds costs per day (more for working at holidays).
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


set of 0..6: DAYS = 0..6; % monday 0 
array[DAYS] of int: Need;
array[DAYS] of int: Cost;

% decision variables. x[i]: No. of workers starting at day i
array[DAYS] of var 0..6: x;

% objective function: total cost
var 0..20000: z = sum(i in DAYS) (x[i] * Cost[i]); 
var 0..100: num_workers = sum(x);

solve :: int_search([z] ++ x, first_fail, indomain_min, complete) minimize z;
% solve :: int_search([z] ++ x, first_fail, indomain_min, complete) minimize num_workers;
% solve satisfy;


constraint 
  forall(i in DAYS) (
    sum(j in DAYS where j != (i+5) mod 7 /\ j != (i+6) mod 7) (x[j]) >= Need[i]
  )
  % /\
  % num_workers = 23
;

%
% data
%

Need = array1d(DAYS,[17, 13, 15, 19, 14, 16, 11]);

% Total cost for the 5 day schedule.
% Base cost per day is 100.
% Working saturday is 100 extra
% Working sunday is 200 extra.
Cost = array1d(DAYS,[500, 600, 800, 800, 800, 800, 700]);


output 
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" ++
  "num_workers: " ++ show(num_workers) ++ "\n"
];
