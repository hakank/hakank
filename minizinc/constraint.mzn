% 
% Optimizing a Constraint problem in MiniZinc.
% 
% From H. Paul Williams "Model Building in Mathematical Programming", 4th edition
% Optimizing a constraint, sections 12.18, 13.18 and 14.18.
%


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of int: terms = 1..8; % variables in constraint 	
set of int: ceils = 1..6; 
set of int: roofs = 1..6;

array[ceils, terms] of int: ceiling;
array[roofs, terms] of int: roofing;

% array[terms] of var int: a; % new coefficients
array[terms] of var 1..1000: a; % new coefficients
var int: b;                 % new right-hand-side

var int: rhs = b - a[3] - a[5]; % minimise new right-hand-side
% solve :: int_search(
%       a ++ [b,rhs],
%      "occurrence", 
%      "indomain_min",
%      "complete"
%    )
%    minimize rhs;

% alternative objective: minimise sum of new coefficients
var int: ss = sum(j in terms) (a[j]*j);
solve :: int_search(
      a ++ [b,rhs],
     first_fail, % occurrence, 
     indomain_min,
     complete
   )
   minimize ss;


constraint
   forall(i in terms) (
     a[i] >= 0
   )
   /\
   b >= 0
   /\
   rhs >= 0
   /\
   forall(i in ceils) (
        sum(j in terms where ceiling[i,j]>0) (a[ceiling[i,j]]) <= b
   )
   /\
   forall(k in roofs) (
        sum(j in terms where roofing[k,j]>0) (a[roofing[k,j]]) >= b+1
   ) 
   /\
   forall(j in terms where j < card(terms)) (
        a[j]-a[j+1] >= 0
   )
;

output [
  "b: ", show(b),"\n",
  "rhs: ", show(rhs), "\n",
  "ss: ", show(ss), "\n",
  "a: ", show(a), "\n",

];

%
% data
% 
ceiling = array2d(ceils, terms, [
1, 2, 3, 0, 0, 0, 0, 0,
1, 2, 4, 8, 0, 0, 0, 0,
1, 2, 6, 7, 0, 0, 0, 0,
1, 3, 5, 6, 0, 0, 0, 0,
2, 3, 4, 6, 0, 0, 0, 0,
2, 5, 6, 7, 8,0, 0, 0]);

roofing = array2d(roofs, terms, 
[
1, 2, 3, 8, 0, 0, 0, 0,
1, 2, 5, 7, 0, 0, 0, 0,
1, 3, 4, 7, 0, 0, 0, 0,
1, 5, 6, 7, 8, 0, 0, 0,
2, 3, 4, 5, 0, 0, 0, 0,
3, 4, 6, 7, 8, 0, 0, 0]);

