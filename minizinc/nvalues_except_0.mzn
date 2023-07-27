% 
% Global constraint nvalues_except_0 in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnvalues_except_0.html
% """
% Constraint
%
%     nvalues_except_0(VARIABLES,RELOP,LIMIT)​
%
% Purpose
%
%     Let N be the number of distinct values, different from 0, assigned 
%     to the variables of the VARIABLES collection. Enforce condition 
%     N RELOP LIMIT to hold.
%
% Example
%     (<4,5,5,4,0,1>,=,3)
%
%     The nvalues_except_0 constraint holds since the number of distinct 
%     values, different from 0, occurring within the collection 
%     <4,5,5,4,0,1> is equal (i.e., RELOP is set to =) to its 
%     third argument LIMIT=3.
% """
%
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn";

int: n = 6;
array[1..n] of var 0..n: variables; 
var 1..n: limitx;
-2..2: relop = 0; % the comparison operator


%
% nvalues_except_0: counts the different values (except 0) in array x
% 
% Since MiniZinc don't handle relational operators (e.g. <, = , >) as 
% arguments in  predicates, we use a method of coding these operators as:
%
%   <  : -2 
%   <= : -1
%    = :  0
%   >= :  1
%   >  :  2
% 
% Note: If relop is not 0 (=) and nv not fixed with '=', then more 
% than one solutions for the same x may be generated. 
% This may be considered a bug or a feature.
%
predicate nvalues_except_0(array[int] of var int: x, int: relop, var int: nv) = 
  let {
     var set of lb_array(x)..ub_array(x): s
  }
  in
  forall(i in lb_array(x)..lb_array(x)) (
    (not exists(j in index_set(x)) (x[j] != 0 /\ x[j] = i ) <-> not (i in s))
  )
  /\
  if relop = -2     then 
     card(s) < nv 
  elseif relop = -1 then
     card(s) <= nv
  elseif relop = 0  then
     card(s) = nv 
  elseif relop = 1  then
     card(s) >= nv
  elseif relop = 2  then
     card(s) > nv 
  else
     false
  endif
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint

  limitx = 3 /\
  cp1d(variables, [4,5,5,4,0,1]) /\
  nvalues_except_0(variables, relop, limitx)

  % /\ testing when variables is free
  % sum(i in index_set(variables)) (bool2int(variables[i] = 0)) = 3

;


output [
"variables: " ++ show(variables) ++ "\n"++
"relop: " ++ show(relop) ++ "\n"++
"limitx: " ++ show(limitx) ++ "\n"
];
