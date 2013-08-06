% 
% Mr Smith problem in MiniZinc.
% 
% From an IF Prolog example (http://www.ifcomputer.de/)
% """
% The Smith family and their three children want to pay a visit but they
% do not all have the time to do so. Following are few hints who will go
% and who will not:
%     o If Mr Smith comes, his wife will come too.
%     o At least one of their two sons Matt and John will come.
%     o Either Mrs Smith or Tim will come, but not both.
%     o Either Tim and John will come, or neither will come.
%     o If Matt comes, then John and his father will
%       also come.
%  """
%
% The answer should be:
%  Mr_Smith_comes      =  0
%  Mrs_Smith_comes     =  0
%  Matt_comes          =  0
%  John_comes          =  1
%  Tim_comes           =  1
%
% Compare with the following models:
% * ECLiPSe: http://www.hakank.org/eclipse/mr_smith.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/mr_smith.pl
% * Gecode: http://www.hakank.org/gecode/mr_smith.cpp


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
var bool: Mr_Smith;
var bool: Mrs_Smith;
var bool: Matt;
var bool: John;
var bool: Tim;

array[1..5] of var bool: x = [Mr_Smith, Mrs_Smith, Matt, John, Tim];

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   % If Mr Smith comes /\ his wife will come too.
   (Mr_Smith -> Mrs_Smith) /\

   % At least one of their two sons Matt and John will come.
   (Matt \/ John) /\

   % Either Mrs Smith or Tim will come /\ but not both.
   % bool2int(Mrs_Smith) + bool2int(Tim) = 1 /\
   (Mrs_Smith xor Tim) /\

   % Either Tim and John will come /\ or neither will come.
   Tim = John /\

   % If Matt comes /\ then John and his father will also come.
   (Matt -> (John /\ Mr_Smith))
;

output
[
  show(x)
];
