% 
% Global constraint sum_ctr in MiniZinc.
%
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Csum_ctr.html
% """
%
% sum_ctr(VARIABLES, CTR, VAR)
% 
% Constraint the sum of a set of domain variables. More precisely, let S denote the sum of the 
% variables of the VARIABLES collection. Enforce the following constraint to hold: S CTR VAR.
% 
% [CTR are: [=, !=, <, >=, > , <=]
% 
% Example
%  (<1, 1,4>, =, 6)
% 
% The sum_ctr constraint holds since the condition 1+1+4=6 is satisfied.
% """
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 3;
array[1..n] of var 1..5: x;
var int: s; % the sum

%
% sum_ctr
% 
% CTR coding:
%  -2:  <
%  -1:  <=
%   0:  =
%   1:  >=
%   2:  >
% else: !=
%
predicate sum_ctr(array[int] of var int: x, int: ctr, var int: s) =
    let {
       var int: ss = sum(x)
    }
    in
    if     ctr = -2 then
       ss < s
    elseif ctr = -1 then
       ss <= s
    elseif ctr =  0 then
       ss = s
    elseif ctr =  1 then
       ss >= s
    elseif ctr =  2 then
       ss > s
    else 
       ss != s
    endif
;

solve satisfy;

constraint
%   x = [1,1,4]
%   /\
   sum_ctr(x, 12, s)
   /\ s = 6

;



output
[
 "x: " ++ show(x) ++ "\n" ++
 "s: " ++ show(s) ++ "\n"
];
