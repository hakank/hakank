% 
% The Interns problem in MiniZinc.
% 
% A classic theorem prover problem.
% 
% From TPTP #18
% http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Problems&Domain=PUZ&File=PUZ018_1.p
% """
% File     : PUZ018_1 : TPTP v6.1.0. Bugfixed v5.0.0.
% Domain   : Puzzles
% Problem  : The Interns
% Version  : Especial.
% English  : Three interns are residents of the same hospital. On only one day
%            of the week are all three interns on call. No intern is on call on
%            three consecutiveutive days. No two interns are off on the same
%            day more than once a week. The first intern is off on Sunday,
%            Tuesday, and Thursday. The second intern is off on Thursday and
%            Saturday. The third intern is off on Sunday.  Which day of the
%            week are all three interns on call?
% """

% This problem has a unique solution:
%
%             s m t w t f s
%   intern 1: o w o w o w w
%   intern 2: w o w w o w o
%   intern 3: o w w o w w o
%
%
% Without the specific info about when an intern is off, 
% there are 1464 solutions.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: sunday    = 1;
int: monday    = 2;
int: tuesday   = 3;
int: wednesday = 4;
int: thursday  = 5;
int: friday    = 6;
int: saturday  = 7;

/*
% Note: This week setting yield two solutions.
int: monday    = 1;
int: tuesday   = 2;
int: wednesday = 3;
int: thursday  = 4;
int: friday    = 5;
int: saturday  = 6;
int: sunday    = 7;
*/

% Three interns are residents of the same hospital. 
int: n = 3; % number of interns
int: w = 7; % number of days in a week

% 0=o(ff), 1=w(ork)
array[0..1] of string: d = array1d(0..1, ["o","w"]);

% decision variables

array[1..n, 1..w] of var 0..1: interns;

solve satisfy;

constraint

  % On only one day of the week are all three interns on call. 
  sum(j in 1..w) ( 
    bool2int(sum(i in 1..n) (interns[i,j]) = 3)
  ) = 1

  % No intern is on call on three consecutive days. 

  % /\
  % forall(i in 1..n) (
  %   forall(j in 1..w-2) (
  %     sum([interns[i,j+k] | k in 0..2 ]) <= 2
  %   ) 
  % )
  /\ 
  forall(i in 1..n) (
     % for each 3-slice there can be between 0 and 2 calls (1's)
     sliding_sum(0,2,3, [interns[i,j] | j in 1..w] )
  )

  % No two interns are off on the same day more than once a week. 
  /\
  forall(i1, i2 in 1..n where i1 < i2) (
    sum(j in 1..w) (
       bool2int(interns[i1,j] = 0 /\ interns[i2,j] = 0)
    ) = 1
  )

  % The first intern is off on Sunday, Tuesday, and Thursday. 
  /\
  interns[1,sunday] = 0 /\
  interns[1,tuesday] = 0 /\
  interns[1,thursday] = 0

  % The second intern is off on Thursday and Saturday. 
  /\
  interns[2,thursday] = 0 /\
  interns[2,saturday] = 0

  % The third intern is off on Sunday.  
  /\ 
  interns[3,sunday] = 0

  % Which day of the week are all three interns on call?

;


output 
[
  "          s m t w t f s"
]
++
[
  if j = 1 then "\nintern " ++ show(i) ++ ": " else " " endif ++
    show(d[fix(interns[i,j])])
  | i in 1..n, j in 1..w
];

