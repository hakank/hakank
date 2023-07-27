% 
% Implementation (and small test) of the global constraints 
%    - nvalues 
% in Minizinc.
%
% This is a generalization of nvalues (see nvalues.mzn).
%
% Reference: 
% Clobal Constraint Catalog
% https://sofdem.github.io/gccat/gccat/Cnvalues.html
% """
% Purpose
%
%     Let N be the number of distinct values assigned to the variables of the VARIABLES collection. 
%     Enforce condition N RELOP LIMIT to hold.
%
% Example
%     (4,5,5,4,1,5,=,3)​
%
%     The nvalues constraint holds since the number of distinct values occurring within the 
%     collection 4,5,5,4,1,5 is equal (i.e., RELOP is set to =) to its third argument LIMIT=3.

% """
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn";

int: n = 6;
array[1..n] of var 1..n: x; 
var 1..n: nvv; % the count of different values in x

%
% nvalues: counts the different values in array x
% 
% Since MiniZinc don't handle relational operators (e.g. <, = , >) as arguments in 
% predicates, we use a method of coding these operators as:
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
% 
predicate nvalues(array[int] of var int: x, int: relop, var int: nv) = 
  let {
     var set of lb_array(x)..ub_array(x): s
  }
  in
  % all values in x should be in s
  forall(i in index_set(x)) (
     x[i] in s
  )
  /\ 
  % the values _not_ in x is _not_ in s
  forall(i in lb_array(x)..ub_array(x)) (
    (not exists(j in index_set(x)) (x[j] = i ) <-> not (i in s))
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


% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;
solve satisfy;

constraint

  % The example for nvalues from global constraint site. 
  cp1d(x, [4,5,5,4,1,5]) /\
  nvalues(x, 0, nvv)

  % nvv may be used with a relational operator other than =, but 
  % could then give confusing results. 
  % Change the relational operator in nvalues instead.
  % /\ nvv = n

  %  /\ increasing(x) % symmetry breaking
;

output [
  "nv: ", show(nvv), " x: ", show(x), "\n"
];

